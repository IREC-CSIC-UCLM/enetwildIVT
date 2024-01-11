# enetwildIVT

enetwildIVT is an R package that contain a golemized shinyapp to validate the harmonized databases provided by the partners of the ENEWTILD proyect.

## Installation

To install locally the Dockerized ShinyApp go to the releases section on this repository and download from the latest one:

- .tar.gz file containing the shinyapp as an Rpackage
- Dockerfile for dependencies
- README with commands

## Usage

To use this docker image you first need to have installed docker, here you can find more information regarding Docker instalation: https://docs.docker.com/engine/install/

Once you have it, build the enetwildIVT image using this command:

```sh
sudo docker build -t enetwildivt
```

To run the shinyapp just execute this run image command: 

```sh
sudo docker run -p 3838:3838 -v /opt/enetwildIVT:/opt/enetwildIVT enetwildivt:latest
```

## Issues

Please if you find issues report them on: https://github.com/IREC-CSIC-UCLM/enetwildIVT/issues

## License

This package is licensed under the MIT License. See the LICENSE file for details.
