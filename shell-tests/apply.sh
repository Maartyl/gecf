#!/bin/bash
tail -n +2 $1 | ./gecf $(head -n 1 $1)
