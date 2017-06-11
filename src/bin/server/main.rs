#![feature(try_from)]

extern crate jsonrpc_core;
//extern crate jsonrpc_ipc_server;
extern crate jsonrpc_tcp_server;
extern crate serde_json;

extern crate malk;

use std::sync::{Arc, Mutex};

//use jsonrpc_ipc_server::Server;
use jsonrpc_core::MetaIoHandler;
use jsonrpc_tcp_server::Server;

mod rpc;
mod doc_store;

fn main() {
    let doc_store = Arc::new(Mutex::new(doc_store::DocStore::new()));

	let mut io = MetaIoHandler::<()>::default();

    let doc_store_cloned = doc_store.clone();
	io.add_method("load_buffer", move |params| rpc::load_buffer(&doc_store_cloned, params));

    let doc_store_cloned = doc_store.clone();
	io.add_method("normalise", move |params| rpc::normalise(&doc_store_cloned, params));

    let doc_store_cloned = doc_store.clone();
	io.add_method("edit", move |params| rpc::edit(&doc_store_cloned, params));

	//let server = Server::new("/tmp/json-ipc-test.ipc", io).unwrap();
	let server = Server::new("0.0.0.0:45666".parse().unwrap(), Arc::new(io));
	::std::thread::spawn(move || server.run());
    ::std::thread::sleep(::std::time::Duration::new(1000, 0));
}

