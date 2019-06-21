# Openwhisk HTTP invoke actions, shell script using curl and bash
```curl -v -s -k https://172.17.0.1/api/v1/namespaces/whisk.system/actions/utils/echo?blocking=true\&result=true -X POST -H "Authorization: BaszFmNi00ZWQ1LThjNTQtODE2YWE0ZjhjNTAyOjEyM3pPM3haQ0xyTU42djJCS0sxZFhZRnBYbFBrY2NPRnFtMTJDZEFzTWdSVTRWck5aOWx5R1ZDR3VNREdJd1A=" -H "Content-Type: application/json" -d '{"text":"hello"}'```

# Instructions for Deploying OpenWhisk on Docker container
```
#Deployment has been tested on Ubuntu 14.04
#cd tools/ubuntu-setup && ./all.sh 
#Make sure you are able to get all running docker container using
docker ps
#If not then add yourself to docker group, and relogin
sudo usermod -aG docker $USER
newgrp docker
bash -l
#We will use an ephemeral CouchDB.
#Compile OpenWhisk
cd <home_openwhisk>
./gradlew distDocker
sudo apt-get install python-pip
sudo pip install ansible==2.3.0.0
sudo pip install jinja2==2.9.6
printf "systemProp.http.proxyHost=localhost \n systemProp.http.proxyPort=3128" | cat > ~/.gradle/gradle.properties
cd ansible
#We will use local environment, hence, no need to provide a new environment explicitly.
ansible-playbook setup.yml
ansible-playbook prereq.yml
ansible-playbook couchdb.yml
ansible-playbook initdb.yml
ansible-playbook wipe.yml
ansible-playbook apigateway.yml
ansible-playbook openwhisk.yml
ansible-playbook postdeploy.yml
#Check that different docker containers are running using
docker ps
#Go to directory where CLI Binaries are
cd <openwhisk_home>/bin
#Set 172.17.0.1 as api host address and auth key
./wsk property set --apihost 172.17.0.1
#Auth key can be manually setup to 23bc46b1-71f6-4ed5-8c54-816aa4f8c502:123zO3xZCLrMN6v2BKK1dXYFpXlPkccOFqm12CdAsMgRU4VrNZ9lyGVCGuMDGIwP
./wsk property set --auth 23bc46b1-71f6-4ed5-8c54-816aa4f8c502:123zO3xZCLrMN6v2BKK1dXYFpXlPkccOFqm12CdAsMgRU4VrNZ9lyGVCGuMDGIwP
#Check if list namespaces contains a guest namespace, otherwise there is something wrong.
./wsk -i namespace list
```
# To Redeploy Openwhisk after changing source code or a restart
```
cd <openwhisk_home>
./gradlew distDocker
cd ansible/ && ansible-playbook -i environments/local openwhisk.yml -e mode=clean 
ansible-playbook -i environments/local openwhisk.yml
```

# OpenWhisk
To redeploy OpenWhisk execute
	/gradlew distDocker && cd ansible/ && ansible-playbook -i environments/local openwhisk.yml -e mode=clean && ansible-playbook -i environments/local openwhisk.yml
Incase of nginx error, ```sudo killall nginx```
[![Build Status](https://travis-ci.org/apache/incubator-openwhisk.svg?branch=master)](https://travis-ci.org/apache/incubator-openwhisk)
[![License](https://img.shields.io/badge/license-Apache--2.0-blue.svg)](http://www.apache.org/licenses/LICENSE-2.0)
[![Join Slack](https://img.shields.io/badge/join-slack-9B69A0.svg)](http://slack.openwhisk.org/)

OpenWhisk is a cloud-first distributed event-based programming service. It provides a programming model to upload event handlers to a cloud service, and register the handlers to respond to various events. Learn more at [http://openwhisk.incubator.apache.org](http://openwhisk.incubator.apache.org).


* [Quick Start](#quick-start) (Vagrant)
* [Native development](#native-development) (Mac and Ubuntu)
* [Learn concepts and commands](#learn-concepts-and-commands)
* [Issues](#issues)
* [Slack](#slack)

### Quick Start

A [Vagrant](http://vagrantup.com) machine is the easiest way to run OpenWhisk on Mac, Windows PC or GNU/Linux.
Download and install [VirtualBox](https://www.virtualbox.org/wiki/Downloads) and [Vagrant](https://www.vagrantup.com/downloads.html) for your operating system and architecture.

**Note:** For Windows, you may need to install an ssh client in order to use the command `vagrant ssh`. Cygwin works well for this, and Git Bash comes with an ssh client you can point to. If you run the command and no ssh is installed, Vagrant will give you some options to try.

Follow these step to run your first OpenWhisk Action:
```
# Clone openwhisk
git clone --depth=1 https://github.com/apache/incubator-openwhisk.git openwhisk

# Change directory to tools/vagrant
cd openwhisk/tools/vagrant

# Run script to create vm and run hello action
./hello
```

Wait for hello action output:
```
wsk action invoke /whisk.system/utils/echo -p message hello --result
{
    "message": "hello"
}
```

These steps were tested on Mac OS X El Capitan, Ubuntu 14.04.3 LTS and Windows using Vagrant.
For more information about using OpenWhisk on Vagrant see the [tools/vagrant/README.md](tools/vagrant/README.md)

### Native development

Docker must be natively installed in order to build and deploy OpenWhisk.
If you plan to make contributions to OpenWhisk, we recommend either a Mac or Ubuntu environment.

* [Setup Mac for OpenWhisk](tools/macos/README.md)
* [Setup Ubuntu for OpenWhisk](tools/ubuntu-setup/README.md)

### Learn concepts and commands

Browse the [documentation](docs/) to learn more. Here are some topics you may be
interested in:

- [System overview](docs/about.md)
- [Getting Started](docs/README.md)
- [Create and invoke actions](docs/actions.md)
- [Create triggers and rules](docs/triggers_rules.md)
- [Use and create packages](docs/packages.md)
- [Browse and use the catalog](docs/catalog.md)
- [Using the OpenWhisk mobile SDK](docs/mobile_sdk.md)
- [OpenWhisk system details](docs/reference.md)
- [Implementing feeds](docs/feeds.md)

### Repository Structure

The OpenWhisk system is built from a number of components.  The picture below groups the components by their GitHub repos. Please open issues for a component against the appropriate repo (if in doubt just open against the main openwhisk repo).

![component/repo mapping](docs/images/components_to_repos.png)

### Issues

Report bugs, ask questions and request features [here on GitHub](../../issues).

### Slack

You can also join the OpenWhisk Team on Slack [https://openwhisk-team.slack.com](https://openwhisk-team.slack.com) and chat with developers. To get access to our public slack team, request an invite [https://openwhisk.incubator.apache.org/slack.html](https://openwhisk.incubator.apache.org/slack.html).
