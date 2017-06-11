use serde_json;
use malk::parse::document::{Diff, Document};
use jsonrpc_core::{Value, Error, Params};

use doc_store::{DocStore, DocId};

use std::sync::{Arc, Mutex};

use self::util::{get_string, get_u64, get_usize};

mod util;

pub type Map = serde_json::Map<String, Value>;

pub enum DaemonToEditor {
    // remove this once we make ide <-> daemon comms async
    Nothing {
        // for debuging rpc
        full_text: String,
    },

    // Finished loading the doc.
    Loaded {
        doc_id: DocId,
    },

    // replace any piece of matching text with the new text.
    ApplyDiff {
        doc_id: DocId,
        start_pos: usize,
        remove_len: usize,
        insert_text: String,
    },
}

impl DaemonToEditor {
    pub fn encode(self) -> Value {
        match self {
            DaemonToEditor::Nothing { full_text } => {
                let mut map = Map::new();
                map.insert(String::from("kind"), Value::String(String::from("nothing")));
                map.insert(String::from("full_text"), Value::String(full_text));
                Value::Object(map)
            },
            DaemonToEditor::Loaded { doc_id } => {
                let mut map = Map::new();
                map.insert(String::from("kind"), Value::String(String::from("loaded")));
                map.insert(String::from("doc_id"), Value::Number(From::from(doc_id)));
                Value::Object(map)
            },
            DaemonToEditor::ApplyDiff { doc_id, start_pos, remove_len, insert_text } => {
                let mut map = Map::new();
                map.insert(String::from("kind"), Value::String(String::from("apply_diff")));
                map.insert(String::from("doc_id"), Value::Number(From::from(doc_id)));
                map.insert(String::from("start_pos"), Value::Number(From::from(start_pos)));
                map.insert(String::from("remove_len"), Value::Number(From::from(remove_len)));
                map.insert(String::from("insert_text"), Value::String(insert_text));
                Value::Object(map)
            },
        }
    }
}

pub fn load_buffer(doc_store: &Arc<Mutex<DocStore>>, params: Params) -> Result<Value, Error> {
    let mut doc_store = doc_store.lock().unwrap();
    let mut map = match params {
        Params::Map(map) => map,
        _ => {
            return Err(Error::invalid_params("expected a map"));
        },
    };

    let text = get_string(&mut map, "text")?;

    let doc_id = doc_store.load(text);

    let ret = DaemonToEditor::Loaded { doc_id: doc_id }.encode();
    Ok(ret)
}

fn open_params<F>(doc_store: &Arc<Mutex<DocStore>>, params: Params, f: F) -> Result<Value, Error>
    where F: FnOnce(&mut Document, DocId, Map) -> Result<Value, Error>
{
    let mut doc_store = doc_store.lock().unwrap();

    let mut map = match params {
        Params::Map(map) => map,
        _ => {
            return Err(Error::invalid_params("expected a map"));
        },
    };

    let doc_id = get_u64(&mut map, "doc_id")?;

    let document = match doc_store.get_doc(doc_id) {
        Some(document) => document,
        None => {
            return Err(Error::invalid_params("invalid document id"));
        },
    };

    f(document, doc_id, map)
}

pub fn normalise(doc_store: &Arc<Mutex<DocStore>>, params: Params) -> Result<Value, Error> {
    let ret = open_params(doc_store, params, |mut document, doc_id, mut map| {
        let pos = get_usize(&mut map, "pos")?;
        let ret = match document.normalise(pos) {
            Some(diff) => {
                DaemonToEditor::ApplyDiff {
                    doc_id: doc_id,
                    start_pos: diff.start_pos,
                    remove_len: diff.remove_len,
                    insert_text: diff.insert_text,
                }
            }
            None => DaemonToEditor::Nothing { full_text: document.text().to_owned() },
        };
        Ok(ret.encode())
    });
    println!("normalise result: {:?}", ret);
    ret
}

pub fn edit(doc_store: &Arc<Mutex<DocStore>>, params: Params) -> Result<Value, Error> {
    open_params(doc_store, params, |mut document, _doc_id, mut map| {
        let stamp = get_usize(&mut map, "stamp")?;
        let start_pos = get_usize(&mut map, "start_pos")?;
        let remove_len = get_usize(&mut map, "remove_len")?;
        let insert_text = get_string(&mut map, "insert_text")?;
        let diff = Diff {
            start_pos: start_pos,
            remove_len: remove_len,
            insert_text: insert_text,
        };
        document.apply_diff(diff, stamp);
        Ok(DaemonToEditor::Nothing {
            full_text: document.text().to_owned(),
        }.encode())
    })
}

