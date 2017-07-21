# Horn-Clause-Resolution

Horn clauses (HC) are sentences in the format A ^ B ^ C -> D.
If true, this sentence is also true:        ~A v ~B v ~C v D.

HC-resolution takes a query in the format ~D v ~E and finds a HC in the knowledge base (KB) to resolve with. It must find an atomic sentence which is either identical or can be made identical with valid substitutions. Once found substitutions are applied, if necessary, and resolution is made to form a new query. For instance:

HC: ~A v ~B v ~C v D
resolves with
Query: ~D v ~E
to form the new query
~A v ~B v ~C v ~E
eliminating the atomic sentence D.
