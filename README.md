# ⚒ Algoritmos Aprendizado Máquina

Esse trabalho é a continuação do trabalho de pré-processamento e análise de dados e tem como objetivo utilizar três algoritmos de aprendizado de máquina para desenvolver uma análise preditiva sobre a base de dados do câncer de mama que já foi analisada e pré-processada.

Os três algoritmos utilizados foram: 
* KNN
* MLP
* Árvore de Decisão

Para utilização desses 3 algoritmos, foi preciso escolher uma técnica de validação a escolhida por mim foi a `cross-validation`, depois também foi necessário definir algumas métricas no meu caso utilizei a matriz de confuso, acurácia precisão e recall e também foi feito um algoritmo de baseline para ajudar na validação dos 3 algoritmos de aprendizado de máquina.

Para mais detalhes é possível visualizar o relatório disponível aqui no repositório, ou [*Clicando aqui*](https://github.com/MariaClaraSanchez/Algoritmos-Aprendizado-M-quina/blob/main/relat%C3%B3rio.pdf)<br>

# 🗺️ Linguagem Utilizada:

![GitHub language count](https://img.shields.io/badge/R-276DC3?style=for-the-badge&logo=r&logoColor=white)

Projeto foi desenvolvido utilizado puramente a linguagem R, que é ótima para análise e processamento de dados, bem como a geração de gráficos.

# ⚙️ Configurações
Esse projeto foi construído utilizado o ambiente de desenvolvimento RStudio, por isso a recomendação abaixo será para que seja instalado ele e a versão do R utilizada para o desenvolvimento desse projeto foi o R.4.2.1, por isso ele também foi sugerido abaixo, pois versões mais antigas não tem as bibliotecas que utilizei nele.

## 💻 Pré-requisitos

Antes de começar, você precisa de:

* Instalar a versão `R.4.2.1`;
* Instalar o `RStudio` ou algum ambiente de desenvolvimento que seja compatível;
* Executar o arquivo de pre-processamento [**base_auxiliar.R**](https://github.com/MariaClaraSanchez/Algoritmos-Aprendizado-M-quina/blob/main/base_auxiliar.R)

## 💻 Bibliotecas:

Algumas bibliotecas utilizadas para geração dos gráficos, tabelas e algumas outras funções dentro do código:

* readr 
* caTools
* class
* RSNNS
* tree
