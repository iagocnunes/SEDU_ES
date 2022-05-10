<!-- Header -->
<p align="center">
  <a href="http://www.ijsn.es.gov.br/">
    <img src="https://i.ibb.co/8dHCR9V/ijsn-govestadual.png" width="940" alt="Instituto Jones dos Santos Neves">
  </a>
</p>

# Processamento de dados

## Análise, limpeza e união de dados

Aqui estão rotinas de tratamento de dados para as pesquisas do [Instituto Jones dos Santos Neves (IJSN)](http://www.ijsn.es.gov.br/) sobre o tamanho do efeito do estudo em tempo integral no aprendizado e abandono (2014-2020) e sobre a classificação de estudantes como em risco de abandono (2016-2020).
Para a primeira, a limpeza e a união são para os dados de Rendimento Escolar, coletados na segunda etapa do Censo Escolar da Educação Básica do INEP, e os dados de Matrículas Iniciais e Escolas, da primeira etapa do Censo Escolar.
Já para a segunda pesquisa, a rotina de limpeza e união ainda inclui os dados administrativos internos da [Secretaria de Estado da Educação do Espírito Santo (SEDU)](https://sedu.es.gov.br/).
A união dos dados sobre Docentes, da primeira etapa do Censo Escolar, ainda estão sobre construção devido a um problema encontrado nas bases do INEP - como o INEP encerrou a divulgação dos microdados públicos, possivelmente a assistência técnica para pesquisadores também fora encerrada; sendo assim, ainda estão em estudo critérios de seleção para contornar esses casos, uma vez que inserir esses problemas nos modelos enviesa seus parâmetros.

## Etapas de tratamento

* **Seleção da população de referÃªncia**
* **Seleção de variáveis de interesse**
* **Tratamento de informações faltantes**
* **Tratamento de Repetições**
  - **Repetições por notas, faltas e/ou dias letivos**
  - **Repetições por mudança de turma, turno, escola e/ou município**
  - **Mais de um docente regente de disciplina em turma**
  - **Mais de um gestor por escola**
* **Critérios de exclusão**
  - **Seleção pela data de matrícula e de encerramento de matrícula mais recente**
  - **Seleção pelo valor máximo da codificação do rendimento escolar**
  - **Seleção pela linha com registro mais completo de notas ou faltas**
  - **Seleção aleatorizada**

Os relatórios das pesquisas ainda estão em processo de publicação. Quando se tornarem público, atualizo esse README com o passo-a-passo e os resultados de cada etapa, e os resultados finais.


Pesquisa financiada pela [FAPES - Fundação de Amparo à Pesquisa e Inovação do Espírito Santo](https://fapes.es.gov.br/).
