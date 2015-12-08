// module Node.AWS.AutoScaling

var aws = require('aws-sdk');
var curry = require('curry');

exports.autoScaling = function autoScaling(cfg) {
    return new aws.AutoScaling(cfg);
};

exports.describeAutoScalingGroups = curry(function describeAutoScalingGroups(autoScaling, onError, onSuccess, req) {
    return function () {
        return autoScaling.describeAutoScalingGroups(req, function(e, r) {
            if (e) {
                return onError(e)();
            }
            return onSuccess(r)();
        });
    };
});

exports.setInstanceHealth = curry(function setInstanceHealth(autoScaling, onError, onSuccess, req) {
    return function () {
        return autoScaling.setInstanceHealth(req, function (e, r) {
            if (e) {
                return onError(e)();
            }
            return onSuccess(r)();
        });
    };
});
