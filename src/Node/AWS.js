// module Node.AWS

var aws = require('aws-sdk');

exports.awsConfig = function () {
    return aws.config;
};
