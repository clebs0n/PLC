type Pessoa = String
type Livro = String
type BancoDados = [(Pessoa,Livro)]

baseExemplo :: BancoDados
baseExemplo = [("Sergio","O Senhor dos Aneis"),
               ("Andre","Duna"),
               ("Fernando","Jonathan Strange & Mr.  Norrell"),
               ("Fernando","A Game of Thrones")]

livros :: BancoDados -> Pessoa -> [Livro]
livros bd pessoa = [livro | (p, livro) <- bd, p == pessoa]
emprestimos :: BancoDados -> Livro -> [Pessoa]
emprestimos bd livro = [pessoa | (pessoa, l) <- bd, l == livro]

qtdEmprestimos :: BancoDados -> Pessoa -> Bool
qtdEmprestimos bd livro = emprestimos bd livro /= []




