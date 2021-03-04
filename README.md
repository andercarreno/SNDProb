<h1 align="center">SNDProb</h1>
<h3 align="center">Streaming novelty detection framework for large data stream classification.</h3>

<p align="center">
  <img src="https://img.shields.io/static/v1?label=Language&message=R&color=green&style=plastic&logo=R" alt="Language">
  <img src="https://img.shields.io/badge/Code-Completed-Green?style=plastic" alt="Build status"/>
  <img src="https://img.shields.io/badge/Journal-Under_Revision-blue?style=plastic" alt="Journal status"/>
  <img src="https://img.shields.io/static/v1?label=Author%20Homepage&message=AC&color=blue&style=plastic&link=andercarreno.eus" alt="andercarreno">
  <img src="https://img.shields.io/static/v1?label=Homepage&message=ISG&color=blue&style=plastic&link=http://www.sc.ehu.es/ccwbayes/" alt="ISGWeb">
</p>



<p align="center">
  <img src="figuresPaper/Scenario4_Strategy6.gif" width="300" height="300"/>
</p>

SNDProb provides a Streaming Novelty Detection framework for data stream classification. A Gaussian mixture model is used to model the set of classes. An initial model is learned from a fully labeled dataset. Instances arrive in a stream fashion, one at a time and not necessarily in equal spaced time intervals. SNDProb classifies the newcomer instances among the learned classes based on a probability threshold. When there is no enough evidence to classify an instance among the known classes, it is introduced in a fixed size buffer. When the buffer is full, new classes are sought in this buffer. The Expectation Maximization (EM) algorithm is used to update the current model and discover emerging new classes.


It is a free R code that is under [MIT License](https://github.com/andercarreno/SNDProb/blob/master/LICENSE).

--------

## Usage
The SNDProb framework has 2 main code files. ```SNDProb.R```contains the functions of the SNDProb such as how to learn the initial offline model, predict newcomer instances or discover new classes. ```SNDProbRun.R``` is the main program to run the SNDProb.

In order to simplify the usage of the application for testing purposes. A simple [web application](https://andercarreno.shinyapps.io/SNDProb) has been developed that allows the user to run the experiments explained in the paper.

--------

## Data
The experimental data can be found under the data folder of this repository. The generator functions are under the dataGenerators folder.

In the ```dataGenerators/Scenarios.R``` the arrival strategies can be found.

### Arrival Strategies
| ![](figuresPaper/ArrivalStrategies/StrategiesClassif3_1.png) | ![](figuresPaper/ArrivalStrategies/StrategiesClassif3_2.png)  | ![](figuresPaper/ArrivalStrategies/StrategiesClassif3_3.png)  |
|---|---|---|
| ![](figuresPaper/ArrivalStrategies/StrategiesClassif3_4.png)  | ![](figuresPaper/ArrivalStrategies/StrategiesClassif3_5.png)  | ![](figuresPaper/ArrivalStrategies/StrategiesClassif3_6.png)  |

In order to control the arrival rate of different instances, 6 different _arrival strategies_ have been created. An _arrival strategy_ consists on a variety of exponential functions, one per class, that models the probability of sampling one of the classes at a certain timestamp.

The code that generates the _arrival strategies_ can be found in the ```dataGenerators/scenarios.R```.

### Scenarios
| ![](figuresPaper/Scenarios/ScenariosClassif3_1.png) | ![](figuresPaper/Scenarios/ScenariosClassif3_2.png)  | ![](figuresPaper/Scenarios/ScenariosClassif3_3.png)  |
|---|---|---|
| ![](figuresPaper/Scenarios/ScenariosClassif3_4.png)  | ![](figuresPaper/Scenarios/ScenariosClassif3_5.png)  | ![](figuresPaper/Scenarios/ScenariosClassif3_6.png)  |

A battery of synthetic datasets has been generated to test a variety of realistic situations, and have a fine control over a set of different characteristics. The following aspects have been taken into account:

- the new class emergence timestamp,
- the probability of arrival of instances of new or known classes at each iteration of the stream,
- the overlap between the newcomer class and the existing ones, and
- the shape of the classes.



--------

## Contact
You can contact the author using the following email:
[Ander Carreño](mailto:andercarreno@ehu.eus?subject=[SNDProb]%20Information%20About%20Code)

[![ForTheBadge built-with-science](http://forthebadge.com/images/badges/built-with-science.svg)](https://github.com/andercarreno)
