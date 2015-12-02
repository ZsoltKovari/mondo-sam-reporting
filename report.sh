#!/bin/bash

cd "$( cd "$( dirname "$0" )" && pwd )"

R -f report.R
