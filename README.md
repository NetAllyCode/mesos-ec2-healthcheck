# mesos-ec2-healthcheck

This is a healthcheck script that will set the instance `HealthStatus` property of an AWS AutoScaling instance to `Unhealthy` if the instance is no longer registered as a slave according to a given mesos master

# Setup

## Environment

You'll need bower for dependency management, and pulp, the purescript build tool. They can be installed easily with npm.
```bash
npm install -g bower pulp
```

## Dependencies

To install purescript dependencies run
```bash
bower i
```
Then to install node runtime dependencies, run
```bash
npm install
```

## Compilation

Building the purescript modules is very simple.
```bash
pulp build
```

# Usage

```bash
NODE_PATH=output node app -m http://my-mesos-master:5050 -r us-east-1 -g someGroup -g someOtherGroup
```

You have to set the `NODE_PATH` environment variable to tell node to look in the `output` directory as well as `node_modules` so that it can find the compiled purescript modules.

AWS Credentials may be passed in all the standard ways:
* Through the `~/.aws/credentials` file
* Through the `AWS_SECRET_ACCESS_KEY` and `AWS_ACCESS_KEY_ID` files.

Note that the AWS region must be supplied explicitly as a command line parameter and not just through the `AWS_DEFAULT_REGION` environment variable or other manner. The desired region is represented by the `-r` or `--region` options.

## Docker

The entrypoint of the docker container is, by default, configured to be `node`, and the working directory to be the root of the built and configured application. Therefore, to run `mesos-ec2-healthcheck`, one could do something like the following.
```bash
docker run -e AWS_SECRET_ACCESS_KEY=key -e AWS_ACCESS_KEY_ID=keyid truviewlive/mesos-ec2-healthcheck app -m http://my-mesos-master:5050 -r us-east-1 -g someGroup -g someOtherGroup
```

# Licence

`mesos-ec2-healthcheck` is distributed under the MIT License
