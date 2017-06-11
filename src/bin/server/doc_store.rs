use std::collections::HashMap;

use malk::parse::document::Document;

pub type DocId = u64;

pub struct DocStore {
    next_id: u64,
    documents: HashMap<DocId, Document>,
}

impl DocStore {
    pub fn new() -> DocStore {
        DocStore {
            next_id: 0,
            documents: HashMap::new(),
        }
    }

    pub fn get_doc(&mut self, id: DocId) -> Option<&mut Document> {
        self.documents.get_mut(&id)
    }

    pub fn load(&mut self, text: String) -> DocId {
        let document = Document::load(text);
        let ret = self.next_id;
        self.next_id += 1;
        self.documents.insert(ret, document);
        ret
    }
}

