// module Node.AWS.AutoScaling

var aws = require('aws-sdk');
var curry = require('curry');

exports.autoScaling = function autoScaling(cfg) {
    return new AWS.AutoScaling(cfg);
};

exports.describeAutoScalingGroups = curry(function describeAutoScalingGroups(autoScaling, onError, onSuccess, req) {
    return function () {
        return autoScaling.describeAutoScalingGroups(req, function(e, r) {
            if (e) {
                onError(e)();
            }
            onSuccess(s)();
        });
    };
});
