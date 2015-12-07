// module Node.AWS.EC2

var aws = require('aws-sdk');
var curry = require('curry');

exports.ec2 = function ec2(cfg) {
    return new aws.EC2(cfg);
};

exports.describeInstances = curry(function describeInstances(ec2Client, onError, onSuccess, req) {
    return function () {
        return ec2Client.describeInstances(req, function (e, r) {
            if (e) {
                return onError(e)();
            }
            return onSuccess(r)();
        });
    };
});
