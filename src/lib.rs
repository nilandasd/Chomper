use std::collections::VecDeque;

/*
  if code, then slay
*/

#[derive(Copy, Clone, PartialEq, Eq)]
enum Symbol {
    Op(Operator),
    Input(char)
}

#[derive(Copy, Clone, PartialEq, Eq)]
enum Operator {
    RightParen,
    LeftParen,
    Star,
    Union,
    Concat,
}

#[derive(Copy, Clone, PartialEq)]
struct StateID(usize);

struct NFA_State {
    id: StateID,
    transitions: Vec<(Option<char>, StateID)>,
}

struct DFA_State {
    id:                    StateID,
    equivalent_nfa_states: Vec<StateID>,
    transitions:           Vec<(char, StateID)>,
    accepting:             bool,
}

struct NFA {
    start:  StateID,
    accept: StateID,
}

struct Regex {
    raw_regex:       String,
    tokenized_regex: Vec<Symbol>,
    operator_stack:  VecDeque<Operator>,
    nfa_stack:       VecDeque<NFA>,
    nfa_states:      Vec<NFA_State>,    
    dfa_states:      Vec<DFA_State>,    
    final_nfa:       NFA,
    alphabet:        Vec<char>,
}

impl NFA_State {
    pub fn new(id: usize) -> Self {
        Self {
            id: StateID(id),
            transitions: vec![],
        }
    }
}

impl DFA_State {
    pub fn new(id: usize, equivalent_nfa_states: Vec<StateID>) -> Self {
        Self {
            id: StateID(id),
            equivalent_nfa_states,
            transitions: vec![],
            accepting: false,
        }
    }
}

impl NFA {
    pub fn new(state_a: StateID, state_b: StateID) -> Self {
        Self {
            start: state_a,
            accept: state_b,
        }
    }
}

impl Regex {
    pub fn new() -> Self {
        Self {
            nfa_states: Vec::<NFA_State>::new(),
            dfa_states: Vec::<DFA_State>::new(),
            operator_stack: VecDeque::<Operator>::new(),
            nfa_stack: VecDeque::<NFA>::new(),
            raw_regex: String::new(),
            tokenized_regex: Vec::<Symbol>::new(),
            final_nfa: NFA::new(StateID(0), StateID(0)),
            alphabet: Vec::<char>::new(),
        }
    }

    pub fn from(regex: &str) -> Self {
        return Self::new().set_pattern(regex);
    }

    pub fn set_pattern(mut self, regex: &str) -> Self {
        self.raw_regex = regex.to_string();

        self.process_regex();
        self.parse_regex(); // also creates the nfa
        self.nfa_to_dfa();
        self.reduce_dfa();
        self.clean();

        return self;
    }

    pub fn feed(&mut self, c: char) -> bool {
        todo!()
    }

    pub fn find(&mut self, s: &str) -> bool {
        todo!()
    }

    pub fn reset(&mut self) -> Self {
        todo!()
    }

    fn reduce_dfa(&mut self) {
        todo!()
    }

    /*
     * This function creates a DFA from the NFA, by removing all epsilon
     * transitions, and adding transitions on left out characters to the reject
     * state. This function requires the functions epsilon_closure, and
     * nfa_transitions.
     *
     * The epsilon_closure function finds all the states that can be reached
     * by epsilon moves from a given set of nfa states.
     *
     * The nfa_transitions functions finds all the states that can be reached
     * by a single transition on a certain letter.
     */
    fn nfa_to_dfa(&mut self) {
        let start_epsilon_closure = self.epsilon_closure(vec![self.final_nfa.start]);
        let dfa_start_state = DFA_State::new(self.dfa_states.len(), start_epsilon_closure);
        let dfa_reject_state = DFA_State::new(self.dfa_states.len(), vec![]);
        let dfa_reject_state_id = dfa_reject_state.id;
        let mut prev_added_states = vec![dfa_start_state.id];
        let mut next_added_states = Vec::<StateID>::new();
        let mut add_flag = false;
        let mut dfa_states = Vec::<DFA_State>::new();

        self.dfa_states.push(dfa_start_state);
        self.dfa_states.push(dfa_reject_state);

        loop {
            for dfa_state_id in prev_added_states.iter() {
                for letter in self.alphabet.iter() {
                    let nfa_transition_states = self.nfa_transitions(*letter, &dfa_states[dfa_state_id.0].equivalent_nfa_states);

                    if nfa_transition_states.is_empty() {
                        dfa_states[dfa_state_id.0].transitions.push((*letter, dfa_reject_state_id));
                    }

                    if let Some(state_id) = self.find_dfa_state(&nfa_transition_states) {
                        dfa_states[dfa_state_id.0].transitions.push((*letter, state_id));
                    } else {
                        let new_state = DFA_State::new(dfa_states.len(), nfa_transition_states);

                        add_flag = true;
                        dfa_states[dfa_state_id.0].transitions.push((*letter, new_state.id));
                        next_added_states.push(new_state.id);
                    }
                }
            }

            if !add_flag {
                break;
            }

            add_flag = false;
            prev_added_states = next_added_states.clone();
            next_added_states.clear();
        }

        self.dfa_states = dfa_states;
    }

    /*
     * This function deletes all the unused information used to create the
     * minimized DFA. This includes the NFA.
     *
     */
    fn clean(&mut self) {
        todo!()
    }

    /*
     * Given a set of NFA state ids, find a dfa state with those matching
     * NFA state ids and return its ID. Each DFA state has a unqiue set 
     * of NFA state ids.
     */
    fn find_dfa_state(&self, nfa_state_ids: &Vec<StateID>) -> Option<StateID> {
        for state in self.dfa_states.iter() {
            if state.equivalent_nfa_states.len() != nfa_state_ids.len() {
                continue;
            }

            let mut match_flag = true;
            for state_id in nfa_state_ids.iter() {
                if !state.equivalent_nfa_states.contains(&state_id) {
                    match_flag = false;
                    continue
                }
            }

            if match_flag {
                return Some(state.id);
            }
        }

        return None;
    }

    /*
     * This function finds all the possible states that could be moved to
     * by repeatedly following epsilon transitions.
     */
    fn epsilon_closure(&self, nfa_state_ids: Vec<StateID>) -> Vec<StateID> {
        let mut closure = nfa_state_ids.clone();
        let mut prev_added_states = nfa_state_ids.clone();
        let mut next_added_states = Vec::<StateID>::new();
        let mut add_flag = false;

        loop {
            for id in prev_added_states.iter() {
                let state = &self.nfa_states[id.0];

                for transition in state.transitions.iter() {
                    match transition.0 {
                        None => {
                            if closure.contains(&transition.1) {
                                closure.push(transition.1);
                                next_added_states.push(transition.1);
                                add_flag = true;
                            }
                        }
                        _ => {}
                    }
                }

            }

            if !add_flag {
                return closure;
            }

            prev_added_states = next_added_states.clone();
            next_added_states.clear();
            add_flag = false;
        }
    }

    /* 
     * This function is used in turning the NFA to a DFA.
     * It is used in conjuction with the epsilon_closure function
     * to know all the possible states an NFA could be in after reading 
     * a certain input character.
     */
    fn nfa_transitions(&self, c: char, nfa_state_ids: &Vec<StateID>) -> Vec<StateID> {
        let mut transitions = Vec::<StateID>::new();

        for id in nfa_state_ids {
            let state = &self.nfa_states[id.0];

            for transition in state.transitions.iter() {
                match transition.0 {
                    Some(ch) => {
                        if ch == c {
                            transitions.push(transition.1);
                        }
                    }
                    _ => {}
                }
            }
        }

        return transitions;
    }

    /* This function has 3 main functionalities:
     *    1. Escape characters using backslash.
     *    2. Add concatenation symbols into the string where they are implicit.
     *    3. Detect syntax errors.
     *    4. Convert all operators down to union, concat, and/or star operators.
     *
     * While performing these functions it fills the tokenized_regex vec to be
     * ready for parsing.
     *
     * When to insert a concat operator:
     *    1. aa
     *    2. a(
     *    3. )a
     *    4. )(
     *    5. *a
     *    6. *(
     */
    fn process_regex(&mut self) {
        let mut escape_flag = false;
        let mut concat_flag = false;
        let mut depth = 0;

        for c in self.raw_regex.chars() {
            if escape_flag {
                if concat_flag {
                    self.tokenized_regex.push(Symbol::Op(Operator::Concat));
                }
                concat_flag = true;
                escape_flag = false;
                self.tokenized_regex.push(Symbol::Input(c));
                // add to alphabet
                continue
            }

            if c == '\\' {
                escape_flag = true;
                continue;
            }

            if c == '*' {
                // if that last token was a star, union or left paren error
                concat_flag = true;
                self.tokenized_regex.push(Symbol::Op(Operator::Star));
                return;
            } 
            if c == '|' {
                // if that last token was a |, or left paren error
                concat_flag = false;
                self.tokenized_regex.push(Symbol::Op(Operator::Union));
                return;
            }
            if c == '(' {
                if concat_flag {
                    self.tokenized_regex.push(Symbol::Op(Operator::Concat));
                }
                concat_flag = false;
                self.tokenized_regex.push(Symbol::Op(Operator::LeftParen));
                return;
            }
            if c == ')' {
                // if the last token was a left paren  or | error
                concat_flag = true;
                self.tokenized_regex.push(Symbol::Op(Operator::RightParen));
                return;
            }

            if concat_flag {
                self.tokenized_regex.push(Symbol::Op(Operator::Concat));
            }

            concat_flag = true;
            // add to alphabet
            self.tokenized_regex.push(Symbol::Input(c));
        }
    }

    fn parse_regex(&mut self) {
        self.nfa_states.clear();
        self.nfa_stack.clear();
        self.operator_stack.clear();
        
        for i in 0..self.tokenized_regex.len() {
            let symbol = self.tokenized_regex[i];

            match symbol {
                Symbol::Input(c) => {
                    self.push_nfa(c);
                }
                Symbol::Op(op) => {
                    if self.operator_stack.is_empty() {
                        self.operator_stack.push_back(op);
                        continue;
                    }

                    match op {
                        Operator::LeftParen => {
                            self.operator_stack.push_back(op);
                        }
                        Operator::RightParen => {
                            while *self.operator_stack.back().unwrap() != Operator::LeftParen {
                                self.eval();
                            }

                            self.operator_stack.pop_back(); 
                        }
                        _ => {
                            while !self.operator_stack.is_empty() && self.precedence(op, *self.operator_stack.back().unwrap()) {
                                self.eval();
                            }

                            self.operator_stack.pop_back(); 
                        }
                    }
                }
            }
        }

        self.final_nfa = self.nfa_stack.pop_back().unwrap();
    }

    fn eval(&mut self) {
        let op = self.operator_stack.pop_back().unwrap();

        match op {
            Operator::Star   => self.star(),
            Operator::Concat => self.concat(),
            Operator::Union  => self.union(),
            _ => todo!("error"),
        }
    }

	/* Returns TRUE if precedence of opLeft <= opRight.
	
			Kleens Closure	- highest
			Concatenation	- middle
			Union			- lowest
	*/
    fn precedence(&mut self, opLeft: Operator, opRight: Operator) -> bool {
        if opLeft == opRight {
            return true;
        }

        if opLeft == Operator::Star {
            return false;
        }
        if opRight == Operator::Star {
            return true;
        }
        if opLeft == Operator::Concat {
            return false;
        }
        if opRight == Operator::Concat {
            return true;
        }
        if opLeft == Operator::Union {
            return false;
        }
        if opRight == Operator::Union {
            return true;
        }

        todo!("error");
        assert!(false);
    }

    fn create_nfa_state(&mut self) -> StateID {
        let new_state = NFA_State::new(self.nfa_states.len());

        self.nfa_states.push(new_state);

        return StateID(self.nfa_states.len() - 1);
    }

    fn create_dfa_state(&mut self, equivalent_nfa_states: Vec<StateID>) -> StateID {
        let new_state = DFA_State::new(self.dfa_states.len(), equivalent_nfa_states);

        self.dfa_states.push(new_state);

        return StateID(self.dfa_states.len() - 1);
    }

    fn add_nfa_transition(&mut self, c: Option<char>, state_a: StateID, state_b: StateID) {
        self.nfa_states[state_a.0].transitions.push((c, state_b));
    }

    fn push_nfa(&mut self, c: char) {
        let state_a = self.create_nfa_state();
        let state_b = self.create_nfa_state();

        self.add_nfa_transition(Some(c), state_a, state_b);

        self.nfa_stack.push_back(NFA::new(state_a, state_b));
    }

    fn pop(&mut self) -> Option<NFA> {
        return self.nfa_stack.pop_back();
    }

    fn concat(&mut self) {
        let nfa_b = self.pop().unwrap();
        let nfa_a = self.pop().unwrap();

        self.add_nfa_transition(None, nfa_a.accept, nfa_b.start);

        self.nfa_stack.push_back(NFA::new(nfa_a.start, nfa_b.accept));
    }

    fn star(&mut self) {
        let nfa = self.pop().unwrap();
        let state_a = self.create_nfa_state();
        let state_b = self.create_nfa_state();

        self.add_nfa_transition(None, nfa.accept, nfa.start);
        self.add_nfa_transition(None, state_a, nfa.start);
        self.add_nfa_transition(None, nfa.accept, state_b);
        self.add_nfa_transition(None, state_a, state_b);

        self.nfa_stack.push_back(NFA::new(state_a, state_b));
    }

    fn union(&mut self) {
        let nfa_b = self.pop().unwrap();
        let nfa_a = self.pop().unwrap();
        let state_a = self.create_nfa_state();
        let state_b = self.create_nfa_state();

        self.add_nfa_transition(None, state_a, nfa_a.start);
        self.add_nfa_transition(None, state_a, nfa_b.start);
        self.add_nfa_transition(None, nfa_a.accept, state_b);
        self.add_nfa_transition(None, nfa_b.accept, state_b);

        self.nfa_stack.push_back(NFA::new(state_a, state_b));
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn simple_regex() {
        let regex = Regex::from("foobar");

        assert!(true);
    }
}
