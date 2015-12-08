# mesos-ec2-healthcheck

This is a healthcheck script that will set the instance `HealthStatus` property of an AWS AutoScaling instance to `Unhealthy` if the instance is no longer registered as a slave according to a given mesos master

## Setup

To install purescript dependencies run
```bash
bower i
```

Then to install node runtime dependencies, run
```bash
npm install
```

## Usage

```bash
node app -m http://my-mesos-master:5050 -r us-east-1 -g someGroup -g someOtherGroup
```
