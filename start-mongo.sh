#!/bin/sh
mongod --nounixsocket --fork --dbpath ./mondodata --logpath ./mondodata/log --nohttpinterface --noscripting 
