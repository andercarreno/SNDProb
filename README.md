<h1 align="center">SNDProb</h1>
<h2 align="center">Streaming novelty detection framework for large data stream classification.</h2>

<p align="center">
  <img src="https://img.shields.io/static/v1?label=Language&message=R&color=green&style=plastic&logo=R" alt="Language">
  <img src="https://img.shields.io/badge/Code-Completed-Green?style=plastic" alt="Build status"/>
  <img src="https://img.shields.io/badge/Journal-Under_Revision-blue?style=plastic" alt="Journal status"/>
</p>


SNDProb provides a Streaming Novelty Detection framework for data stream classification. It is a free R code that is under [MIT License](https://github.com/andercarreno/SNDProb/blob/master/LICENSE).

## Usage
A simple [web application](https://andercarreno.shinyapps.io/SNDProb) can be used to run the experiments explained in the paper.

The SNDProb framework has 2 main code files. ```SNDProb.R```contains the functions of the SNDProb such as how to learn the initial offline model, predict newcomer instances or discover new classes. ```SNDProbRun.R``` is the main program to run the SNDProb.

## Data
The experimental data can be found under the data folder of this repository. The generator functions are under the dataGenerators folder.

In the ```dataGenerators/Scenarios.R``` the arrival strategies can be found.

## Contact
You can contact the author using the following email:
[Ander Carreño](mailto:andercarreno@ehu.eus?subject=[SNDProb]%20Information%20About%20Code)
