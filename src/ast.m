% Copyright (C) 2021 (Caden Haustein) <mlatu@brightlysalty.33mail.com>
% This file is part of the Mlatu programming language.
%
% The Mlatu programming language is non-violent software: you can use, 
% redistribute, and/or modify it under the terms of the CNPLv6+ as found
% in the LICENSE file in the source code root directory or
% at <https://git.pixie.town/thufie/CNPL>.
%
% The Mlatu programming language comes with ABSOLUTELY NO WARRANTY, to the 
% extent permitted by applicable law.  See the CNPL for details.

:- module ast.

:- interface.

:- import_module list.

:- import_module context.

:- type m_name == string. 

:- type m_term(Info) ---> mt_call(context, Info, m_name) ; mt_int(context, Info, int) ; mt_def(context, Info, m_name, m_terms(Info)). 

:- type m_terms(Info) ---> mts(context, Info, list(m_term(Info))).

:- type m_spec ---> m_spec(list(m_value), list(m_value)).

:- type m_value ---> mv_int.

:- type term == m_term({}).

:- type terms == m_terms({}).

:- type spec_term == m_term(m_spec).

:- type spec_terms == m_terms(m_spec).

:- func terms_string(m_terms(Info)) = string.

:- func term_string(m_term(Info)) = string.

:- func terms_context(m_terms(Info)) = context.

:- func term_context(m_term(Info)) = context.

:- func terms_spec(spec_terms) = m_spec.

:- func term_spec(spec_term) = m_spec.

:- func spec_value(m_value) = string.

:- func spec_string(m_spec) = string.

:- implementation.

:- import_module string.

terms_string(mts(_, _, List)) = join_list(" ", map(term_string, List)).

term_string(mt_call(_, _, Name)) = Name. 
term_string(mt_int(_, _, Num)) = string.from_int(Num).
term_string(mt_def(_, _, Name, Inner)) = "def " ++ Name ++ " {" ++ terms_string(Inner) ++ "}".

terms_context(mts(Context, _, _)) = Context.

term_context(mt_call(Context, _, _)) = Context. 
term_context(mt_int(Context, _, _)) = Context.
term_context(mt_def(Context, _, _, _)) = Context.

terms_spec(mts(_, Spec, _)) = Spec.

term_spec(mt_call(_, Spec, _)) = Spec. 
term_spec(mt_int(_, Spec, _)) = Spec.
term_spec(mt_def(_, Spec, _, _)) = Spec.

spec_value(mv_int) = "int".

spec_string(m_spec(From, To)) = "(" ++ join_list(" ", map(spec_value, From)) ++ " -> " ++ join_list(" ", map(spec_value, To)) ++ ")".
