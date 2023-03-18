#![allow(dead_code)]

use std::collections::VecDeque;
use std::collections::HashSet;

/*
Quantifiers
    * :  zero or more times
    + :  one or more times
    ? :  zero or one times
    NEXT TO IMPLEMENT _____________
        {a}   : exactly a times
        {a,b} : at least a times and not b or more times
        {a, } : at least a times
        { ,b} : not b or more times
    ______________________________

Special Symbols
    \d : 0-9
    \D :  non-digit character
    \l : [a-zA-Z]
    \L : non-alpha character
    \a : alphanumeric
    \A : non-alphanumeric
    .  : any character
    \  : escape character

Grouping
    () : matching group
    a|b : match expression a or b (union)
 */

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
enum Symbol {
    Op(Operator),
    Transit(Transit)
}

#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash, PartialOrd, Ord)]
enum Transit {
    Char(char),
    Digit,
    Alpha,
    AlphaNumeric,
    NonAlphaNumeric,
    NonDigit,
    NonAlpha,
    Any,
}

#[derive(Debug, Copy, Clone, PartialEq, Eq, PartialOrd)]
enum Operator {
    Star,
    Plus,
    Question,
    Concat,
    Union,
    RightParen,
    LeftParen,
}

#[derive(Copy, Clone, PartialEq)]
struct StateID(usize);

struct NFAstate {
    id: StateID,
    transitions: Vec<(Option<Transit>, StateID)>, // None transit == Epsilon Transition
}

struct DFAstate {
    id:                    StateID,
    equivalent_nfa_states: Vec<StateID>,
    transitions:           Vec<(Transit, StateID)>, // removed option to remove epsilon transitions
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
    nfa_states:      Vec<NFAstate>,    
    dfa_states:      Vec<DFAstate>,    
    final_nfa:       NFA,
    current_state:   Option<StateID>,
}

impl NFAstate {
    pub fn new(id: usize) -> Self {
        Self {
            id: StateID(id),
            transitions: vec![],
        }
    }
}

impl DFAstate {
    pub fn new(id: usize, equivalent_nfa_states: Vec<StateID>, accepting: bool) -> Self {
        Self {
            id: StateID(id),
            equivalent_nfa_states,
            transitions: vec![],
            accepting,
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
            operator_stack:  VecDeque::<Operator>::new(),
            nfa_stack:       VecDeque::<NFA>::new(),
            final_nfa:       NFA::new(StateID(0), StateID(0)),
            nfa_states:      Vec::<NFAstate>::new(),
            dfa_states:      Vec::<DFAstate>::new(),
            raw_regex:       String::new(),
            tokenized_regex: Vec::<Symbol>::new(),
            current_state:   None
        }
    }

    pub fn from(regex: &str) -> Self {
        return Self::new().set_pattern(regex);
    }

    pub fn set_pattern(mut self, regex: &str) -> Self {
        self.raw_regex = regex.to_string();

        self.process_regex();
        self.parse_regex();
        self.nfa_to_dfa();
        self.reduce_dfa();
        self.clean();

        return self;
    }

    fn match_transit(c: &char, transit: &Transit) -> bool {
        match transit {
            Transit::Any             => return true,
            Transit::Digit           => return  c.is_digit(10),
            Transit::NonDigit        => return !c.is_digit(10),
            Transit::Alpha           => return  c.is_alphabetic(),
            Transit::NonAlpha        => return !c.is_alphabetic(),
            Transit::AlphaNumeric    => return  c.is_digit(10) || c.is_alphabetic(),
            Transit::NonAlphaNumeric => return !c.is_digit(10) && !c.is_alphabetic(),
            Transit::Char(ch)  =>       return *c == *ch,
        }
    }

    /*
     *  This function is for giving a regex one character at a time, changing
     *  the state of the internal DFA, and returning true if the DFA has not
     *  rejected the input character given the current state. If no state is set
     *  or the DFA rejects the input, than it returns false.
     */
    pub fn feed(&mut self, c: char) -> bool {
        match self.current_state {
            Some(sid) => {
                let state = &self.dfa_states[sid.0];

                for transition in state.transitions.iter() {
                    if Self::match_transit(&c, &transition.0) {
                        self.current_state = Some(transition.1);
                        return true;
                    }
                }

                self.current_state = None;
                return false;
            }
            None => { return false }
        }
    }

    pub fn restart(&mut self) {
        self.current_state = Some(StateID(0));
    }

    /*
     * Checks if the string is an exact match to the regex.
     */
    pub fn compare(&self, s: &str) -> bool {
        let mut state = &self.dfa_states[0];

        for c in s.chars() {
            let mut reject_flag = true;

            for transition in state.transitions.iter() {
                if Self::match_transit(&c, &transition.0) {
                    state = &self.dfa_states[transition.1.0];
                    reject_flag = false;
                    break;
                }
            }

            if reject_flag {
                return false;
            } 
        }

        return state.accepting;
    }

    /*
     * This function deletes all the unused information used to create the
     * minimized DFA. This includes the NFA.
     */
    fn clean(&mut self) {
        self.nfa_states.clear();
        self.tokenized_regex.clear();
    }

    /*
     * Reduce the dfa by combining matching non accept states,
     * and matching accept states.
     */
    fn reduce_dfa(&mut self) {
        let mut states_to_replace = Vec::<(StateID, StateID)>::new();

        // find states to replace
        for x in 0..self.dfa_states.len() {
            for y in (x+1)..self.dfa_states.len() {
                let state_x = &self.dfa_states[x];
                let state_y = &self.dfa_states[y];

                if state_x.accepting == state_y.accepting
                    && state_x.transitions == state_y.transitions {

                    let mut already_replaced = false;
                    for replacement in states_to_replace.iter() {
                        if replacement.0 == state_x.id {
                            already_replaced = true;
                        }
                    }
                    if already_replaced {
                        continue;
                    }

                    // to keep the start state at dfa_states[0]
                    // always replace the higher id with the lower id
                    states_to_replace.push((state_y.id, state_x.id));
                }
            }
        }

        // replace states (replace id_a with id_b)
        for (id_a, id_b) in states_to_replace {
            let mut remove_index = 0;

            for i in 0..self.dfa_states.len() {
                if self.dfa_states[i].id == id_a {
                    remove_index = i;
                    continue;
                }

                for transition in self.dfa_states[i].transitions.iter_mut() {
                    if transition.1 == id_a {
                        transition.1 = id_b;
                    }
                }
            }

            self.dfa_states.remove(remove_index);
        }

        // readjust table so that state ids match their index
        for i in 0..self.dfa_states.len() {
            if i == self.dfa_states[i].id.0 {
                continue;
            } 

            let old_id = self.dfa_states[i].id;
            self.dfa_states[i].id = StateID(i);
            for k in 0..self.dfa_states.len() {
                for transition in self.dfa_states[k].transitions.iter_mut() {
                    if transition.1 == old_id {
                        transition.1 = StateID(i);
                    }
                }
            }
        }
    }

    /*
     * Finds all possible transits from a aset of nfa states
     */
    fn getTransits(&self, states: &Vec<StateID>) -> Vec<Transit> {
        let mut result = Vec::<Transit>::new();

        for stateid in states.iter() {
            for transit in  self.nfa_states[stateid.0].transitions.iter() {
                match transit.0 {
                    Some(t) => result.push(t),
                    None => {}
                }
            }
        }

        result.sort();

        return result;
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
        let nfa_start_states = self.epsilon_closure(vec![self.final_nfa.start]);
        let accepting = nfa_start_states.contains(&self.final_nfa.accept);
        let dfa_start_state = DFAstate::new(self.dfa_states.len(), nfa_start_states, accepting);
        let mut prev_added_states = vec![dfa_start_state.id];
        let mut next_added_states = Vec::<StateID>::new();
        let mut dfa_states = Vec::<DFAstate>::new();
        let mut add_flag = false;

        dfa_states.push(dfa_start_state);

        loop {
            for dfa_state_id in prev_added_states.iter() {
                let possible_transits = self.getTransits(&dfa_states[dfa_state_id.0].equivalent_nfa_states);

                for transit in possible_transits.iter() {
                    let transition_ids = self.nfa_transitions(*transit, &dfa_states[dfa_state_id.0].equivalent_nfa_states);
                    let nfa_state_ids = self.epsilon_closure(transition_ids);
                    let accepting = nfa_state_ids.contains(&self.final_nfa.accept);

                    if nfa_state_ids.is_empty() {
                        continue;
                    }

                    if let Some(state_id) = self.find_dfa_state(&dfa_states, &nfa_state_ids) {
                        dfa_states[dfa_state_id.0].transitions.push((*transit, state_id));
                        continue;
                    }

                    let new_state = DFAstate::new(dfa_states.len(), nfa_state_ids, accepting);
                    add_flag = true;
                    dfa_states[dfa_state_id.0].transitions.push((*transit, new_state.id));
                    next_added_states.push(new_state.id);
                    dfa_states.push(new_state);
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
     * Given a set of NFA state ids, find a dfa state with those matching
     * NFA state ids and return its ID. Each DFA state has a unqiue set 
     * of NFA state ids.
     */
    fn find_dfa_state(&self, dfa_states: &Vec<DFAstate>, nfa_state_ids: &Vec<StateID>) -> Option<StateID> {
        for state in dfa_states.iter() {
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
                            if !closure.contains(&transition.1) {
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
    fn nfa_transitions(&self, transit: Transit, nfa_state_ids: &Vec<StateID>) -> Vec<StateID> {
        let mut transitions = Vec::<StateID>::new();

        for id in nfa_state_ids {
            let state = &self.nfa_states[id.0];

            for transition in state.transitions.iter() {
                match transition.0 {
                    Some(other_transit) => {
                        if transit == other_transit {
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

                let transit: Transit;
                match c {
                    'd' => transit = Transit::Digit,
                    'D' => transit = Transit::NonDigit,
                    'l' => transit = Transit::Alpha,
                    'L' => transit = Transit::NonAlpha,
                    'a' => transit = Transit::AlphaNumeric,
                    'A' => transit = Transit::NonAlphaNumeric,
                     _  => transit = Transit::Char(c),
                }

                concat_flag = true;
                escape_flag = false;
                self.tokenized_regex.push(Symbol::Transit(transit));
                continue
            }

            if c == '\\' {
                escape_flag = true;
                continue;
            }

            if c == '.' {
                // if that last token was a star, union or left paren error
                if concat_flag {
                    self.tokenized_regex.push(Symbol::Op(Operator::Concat));
                }

                concat_flag = true;
                self.tokenized_regex.push(Symbol::Transit(Transit::Any));
                continue;
            } 
            if c == '*' {
                // if that last token was a star, union or left paren error
                concat_flag = true;
                self.tokenized_regex.push(Symbol::Op(Operator::Star));
                continue;
            } 
            if c == '+' {
                // if that last token was a star, union or left paren error
                concat_flag = true;
                self.tokenized_regex.push(Symbol::Op(Operator::Plus));
                continue;
            } 
            if c == '?' {
                // if that last token was a star, union or left paren error
                concat_flag = true;
                self.tokenized_regex.push(Symbol::Op(Operator::Question));
                continue;
            } 
            if c == '|' {
                // if that last token was a |, or left paren error
                concat_flag = false;
                self.tokenized_regex.push(Symbol::Op(Operator::Union));
                continue;
            }
            if c == '(' {
                if concat_flag {
                    self.tokenized_regex.push(Symbol::Op(Operator::Concat));
                }
                concat_flag = false;
                self.tokenized_regex.push(Symbol::Op(Operator::LeftParen));
                continue;
            }
            if c == ')' {
                // if the last token was a left paren  or | error
                concat_flag = true;
                self.tokenized_regex.push(Symbol::Op(Operator::RightParen));
                continue;
            }

            if concat_flag {
                self.tokenized_regex.push(Symbol::Op(Operator::Concat));
            }

            concat_flag = true;
            let transit = Transit::Char(c);
            self.tokenized_regex.push(Symbol::Transit(transit));
        }
    }

    /*
     * This function turns the regex into a series of tokens. Tokens can either be 'letters' or operators.
     * Also this function checks the regex for syntax errors.
     */
    fn parse_regex(&mut self) {
        self.nfa_states.clear();
        self.nfa_stack.clear();
        self.operator_stack.clear();
        
        for i in 0..self.tokenized_regex.len() {
            let symbol = self.tokenized_regex[i];

            match symbol {
                Symbol::Transit(t) => {
                    self.push_nfa(t);
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

                            self.operator_stack.push_back(op); 
                        }
                    }
                }
            }
        }

        while !self.operator_stack.is_empty() {
            self.eval();
        }

        self.final_nfa = self.nfa_stack.pop_back().unwrap();
    }

    fn eval(&mut self) {
        let op = self.operator_stack.pop_back().unwrap();

        match op {
            Operator::Star     => self.star(),
            Operator::Plus     => self.plus(),
            Operator::Question => self.question(),
            Operator::Concat   => self.concat(),
            Operator::Union    => self.union(),
            _ => todo!("error"),
        }
    }

	/* Returns TRUE if precedence of opLeft <= opRight.
	
			Kleens Closure	- highest
			Concatenation	- middle
			Union			- lowest
	*/
    fn precedence(&mut self, op_left: Operator, op_right: Operator) -> bool {
        return op_left >= op_right;
    }

    fn create_nfa_state(&mut self) -> StateID {
        let new_state = NFAstate::new(self.nfa_states.len());

        self.nfa_states.push(new_state);

        return StateID(self.nfa_states.len() - 1);
    }

    fn add_nfa_transition(&mut self, t: Option<Transit>, state_a: StateID, state_b: StateID) {
        self.nfa_states[state_a.0].transitions.push((t, state_b));
    }

    fn push_nfa(&mut self, t: Transit) {
        let state_a = self.create_nfa_state();
        let state_b = self.create_nfa_state();

        self.add_nfa_transition(Some(t), state_a, state_b);

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

    /*
    Match zero or more times
    */
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

    /*
    Match one or more times
    */
    fn plus(&mut self) {
        let nfa = self.pop().unwrap();
        let state_a = self.create_nfa_state();
        let state_b = self.create_nfa_state();

        self.add_nfa_transition(None, nfa.accept, nfa.start);
        self.add_nfa_transition(None, state_a, nfa.start);
        self.add_nfa_transition(None, nfa.accept, state_b);

        self.nfa_stack.push_back(NFA::new(state_a, state_b));
    }

    /*
    Match one or zero times
    */
    fn question(&mut self) {
        let nfa = self.pop().unwrap();
        let state_a = self.create_nfa_state();

        self.add_nfa_transition(None, state_a, nfa.start);
        self.add_nfa_transition(None, state_a, nfa.accept);

        self.nfa_stack.push_back(NFA::new(state_a, nfa.accept));
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn compare1() {
        let regex = Regex::from("megaladonkus");

        assert!(!regex.compare(""));
        assert!(!regex.compare("m"));
        assert!(!regex.compare("me"));
        assert!(!regex.compare("meg"));
        assert!(!regex.compare("mega"));
        assert!(!regex.compare("megal"));
        assert!(!regex.compare("megala"));
        assert!(!regex.compare("megalad"));
        assert!(!regex.compare("megalado"));
        assert!(!regex.compare("megaladon"));
        assert!(!regex.compare("megaladonk"));
        assert!(!regex.compare("megaladonku"));
        assert!( regex.compare("megaladonkus")); // MATCH
        assert!(!regex.compare("megaladonkuss"));
        assert!(!regex.compare("megaladonkusss"));
        assert!(!regex.compare("megaladonkussss"));
    }

    #[test]
    fn compare2() {
        //let regex = Regex::from("cheese|please");
        let regex = Regex::from("cheese|please");

        assert!(!regex.compare("cheeseplease"));
        assert!(!regex.compare("pleasecheese"));
        assert!(!regex.compare("chees"));
        assert!(!regex.compare("pleas"));
        assert!(!regex.compare("chease"));
        assert!(!regex.compare(""));

        assert!(regex.compare("cheese"));
        assert!(regex.compare("please"));
    }

    #[test]
    fn compare3() {
        //let regex = Regex::from("cheese|please");
        let regex = Regex::from("(foo|bar)*");

        assert!(!regex.compare("fo"));
        assert!(!regex.compare("ba"));
        assert!(!regex.compare("fooblah"));
        assert!(!regex.compare("barblah"));

        assert!(regex.compare(""));
        assert!(regex.compare("foo"));
        assert!(regex.compare("bar"));
        assert!(regex.compare("foofoo"));
        assert!(regex.compare("barbar"));
        assert!(regex.compare("foobar"));
        assert!(regex.compare("barfoo"));
        assert!(regex.compare("foofoofoo"));
        assert!(regex.compare("foofoobar"));
        assert!(regex.compare("foobarfoo"));
        assert!(regex.compare("foobarbar"));
        assert!(regex.compare("barfoofoo"));
        assert!(regex.compare("barfoobar"));
        assert!(regex.compare("barbarfoo"));
        assert!(regex.compare("barbarbar"));
    }

    #[test]
    fn compare4() {
        let regex = Regex::from("1*2*3*");

        assert!(regex.compare(""));
        assert!(regex.compare("1"));
        assert!(regex.compare("22"));
        assert!(regex.compare("123"));
        assert!(regex.compare("333"));
        assert!(regex.compare("1133"));
        assert!(regex.compare("11122"));
        assert!(regex.compare("233333"));
        assert!(regex.compare("1122233"));

        assert!(!regex.compare("11222334"));
    }
    #[test]
    fn compare5() {
        let regex = Regex::from("((420)*(69)*(1738)*_)*");

        assert!(regex.compare(""));
        assert!(regex.compare("_"));
        assert!(regex.compare("_______"));
        assert!(regex.compare("______420_____"));
        assert!(regex.compare("420_"));
        assert!(regex.compare("69_"));
        assert!(regex.compare("1738_"));
        assert!(regex.compare("420__69__1738__"));
        assert!(regex.compare("1738___420___69___"));

        assert!(!regex.compare("420"));
        assert!(!regex.compare("69"));
        assert!(!regex.compare("1738"));
    }

    #[test]
    fn int_compare() {
        let regex = Regex::from("(1|2|3|4|5|6|7|8|9|0)+");

        assert!(regex.compare("654321"));
        assert!(regex.compare("123456"));

        assert!(!regex.compare(""));
        assert!(!regex.compare("asdfasdf"));
        assert!(!regex.compare("1234asdf"));
    }

    #[test]
    fn float_compare() {
        let regex = Regex::from(r#"-?\d+(\.\d+)?"#);

        assert!(regex.compare("654321"));
        assert!(regex.compare("0.0"));
        assert!(regex.compare("123.456"));
        assert!(regex.compare("-123.456"));
        assert!(regex.compare("0.420420420420420420"));

        assert!(!regex.compare("no"));
        assert!(!regex.compare("blah blah blah blah"));
        assert!(!regex.compare("12414..123123"));
        assert!(!regex.compare("1214."));
        assert!(!regex.compare("-1214."));
        assert!(!regex.compare("-."));
        assert!(!regex.compare("1214.12412."));
    }

    #[test]
    fn string_compare() {
        let regex = Regex::from(r#"".*""#);

        assert!(regex.compare("\"654321\""));
        assert!(regex.compare("\"this is a test to match on strings\""));
        assert!(regex.compare("\"\""));
        assert!(regex.compare("\"s\""));
        assert!(regex.compare("\"sasdfa235908adf(*!&^@$!@%_(*&))@(#%\""));

        assert!(!regex.compare("\"no"));
        assert!(!regex.compare("n\"o\""));
        assert!(!regex.compare("no\""));
        assert!(!regex.compare("blah \"blah\" blah blah"));
        assert!(!regex.compare("blah blah blah blah"));
        assert!(!regex.compare("1214."));
        assert!(!regex.compare("1214.12412."));
    }


    #[test]
    fn any_compare() {
        let regex = Regex::from(".*");

        assert!(regex.compare(""));
        assert!(regex.compare("0.0"));
        assert!(regex.compare("123.456"));
        assert!(regex.compare("-123.456"));
        assert!(regex.compare("0.420420420420420420"));
        assert!(regex.compare("no"));
        assert!(regex.compare("blah blah blah blah"));
        assert!(regex.compare("12414..123123"));
        assert!(regex.compare("1214."));
        assert!(regex.compare("-1214."));
        assert!(regex.compare("-."));
        assert!(regex.compare("1214.12412."));
    }

    #[test]
    fn alpha_compare() {
        let regex = Regex::from(r#"\l*"#);

        assert!(regex.compare(""));
        assert!(regex.compare("yay"));
        assert!(regex.compare("abcdefghijklmnopqrstuvwxyz"));

        assert!(!regex.compare("0.0asdfas"));
        assert!(!regex.compare("1sdfask23.456"));
        assert!(!regex.compare("-123.456"));
        assert!(!regex.compare("0.420420asdfasfdjkkk420420420420"));
        assert!(!regex.compare("blah blah blah blah"));
        assert!(!regex.compare("zxcvzozxcv12414..123123"));
        assert!(!regex.compare("1214."));
        assert!(!regex.compare("-1214."));
        assert!(!regex.compare("-."));
        assert!(!regex.compare("asdfasdsfadf1214.12412."));
    }
}