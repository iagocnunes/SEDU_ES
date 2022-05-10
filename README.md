<!-- Header -->
<p align="center">
  <a href="http://www.ijsn.es.gov.br/">
    <img src="https://i.ibb.co/8dHCR9V/ijsn-govestadual.png" width="940" alt="Instituto Jones dos Santos Neves">
  </a>
</p>

# Processamento de dados

## An�lise, limpeza e uni�o de dados

Aqui est�o rotinas de tratamento de dados para as pesquisas do [Instituto Jones dos Santos Neves (IJSN)](http://www.ijsn.es.gov.br/) sobre o tamanho do efeito do estudo em tempo integral no aprendizado e abandono (2014-2020) e sobre a classifica��o de estudantes como em risco de abandono (2016-2020).
Para a primeira, a limpeza e a uni�o s�o para os dados de Rendimento Escolar, coletados na segunda etapa do Censo Escolar da Educa��o B�sica do INEP, e os dados de Matr�culas Iniciais e Escolas, da primeira etapa do Censo Escolar.
J� para a segunda pesquisa, a rotina de limpeza e uni�o ainda inclui os dados administrativos internos da [Secretaria de Estado da Educa��o do Esp�rito Santo (SEDU)](https://sedu.es.gov.br/).
A uni�o dos dados sobre Docentes, da primeira etapa do Censo Escolar, ainda est�o sobre constru��o devido a um problema encontrado nas bases do INEP - como o INEP encerrou a divulga��o dos microdados p�blicos, possivelmente a assist�ncia t�cnica para pesquisadores tamb�m fora encerrada; sendo assim, ainda est�o em estudo crit�rios de sele��o para contornar esses casos, uma vez que inserir esses problemas nos modelos enviesa seus par�metros.

## Etapas de tratamento

* **Sele��o da popula��o de referência**
* **Sele��o de vari�veis de interesse**
* **Tratamento de informa��es faltantes**
* **Tratamento de Repeti��es**
  - **Repeti��es por notas, faltas e/ou dias letivos**
  - **Repeti��es por mudan�a de turma, turno, escola e/ou munic�pio**
  - **Mais de um docente regente de disciplina em turma**
  - **Mais de um gestor por escola**
* **Crit�rios de exclus�o**
  - **Sele��o pela data de matr�cula e de encerramento de matr�cula mais recente**
  - **Sele��o pelo valor m�ximo da codifica��o do rendimento escolar**
  - **Sele��o pela linha com registro mais completo de notas ou faltas**
  - **Sele��o aleatorizada**

Os relat�rios das pesquisas ainda est�o em processo de publica��o. Quando se tornarem p�blico, atualizo esse README com o passo-a-passo e os resultados de cada etapa, e os resultados finais.


Pesquisa financiada pela [FAPES - Funda��o de Amparo � Pesquisa e Inova��o do Esp�rito Santo](https://fapes.es.gov.br/).
