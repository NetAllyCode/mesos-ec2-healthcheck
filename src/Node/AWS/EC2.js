// module Node.AWS.EC2

var aws = require('aws-sdk');

exports.ec2 = function ec2(cfg) {
    return new aws.EC2(cfg);
};
