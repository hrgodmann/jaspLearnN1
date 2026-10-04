import QtQuick
import JASP.Module

Upgrades {
    Upgrade {
        functionName: "Network"
        fromVersion: "0.1"
        toVersion: "0.1.1"
        ChangeJS {
            name: "problems"
            jsFunction: function(options) {
                var problems = options["problems"] || []
                return problems.map(function(problem) {
                    if (problem["problemSeverityRated"] === undefined)
                        problem["problemSeverityRated"] = true
                    return problem
                })
            }
        }
    }
}
