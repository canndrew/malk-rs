enum MetaVarCtx {
    Nil,
    Cons {
        var_type: Term,
        next: Rc<MetaVarCtx>,
    },
}


struct MetaCtx {
    next_index: u32,
    contexts: HashMap<u32, Rc<MetaVarCtx>>,
}

impl MetaCtx {
    pub fn next_var(&mut self, ctx: &Ctx) -> u32 {
        let next = self.next_index;
        self.next_index += 1;
        next
    }
}

