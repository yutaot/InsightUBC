/**
 * Builds a query object using the current document object model (DOM).
 * Must use the browser's global document object {@link https://developer.mozilla.org/en-US/docs/Web/API/Document}
 * to read DOM information.
 *
 * @returns query object adhering to the query EBNF
 */

CampusExplorer.buildQuery = () => {
    let query = {};
    let activeTabKind = document.getElementsByClassName("nav-item tab active")[0].innerHTML;
    let activeTab;
    let conditionType;
    let keyType;
    if (activeTabKind === "Courses") {
        activeTab = document.getElementById("tab-courses");
        conditionType = CampusExplorer.getConditionType("courses");
        keyType = "courses";
    } else if (activeTabKind === "Rooms") {
        activeTab = document.getElementById("tab-rooms");
        conditionType = CampusExplorer.getConditionType("rooms");
        keyType = "rooms";
    } else {
        return query;
    }
    let conditionGroups = CampusExplorer.getConditionGroups(activeTab);
    let columnsToProject = CampusExplorer.getColumnsToProject(activeTab);
    let sortOrderAndColumnToSortBy = CampusExplorer.getSortOrderAndColumnToSortBy(activeTab);
    let columnsToGroupBy = CampusExplorer.getColumnsToGroupBy(activeTab);
    let transformations = CampusExplorer.getTransformations(activeTab);
    CampusExplorer.translateInputsToBuildQuery(
        query, conditionType, conditionGroups, columnsToProject,
        sortOrderAndColumnToSortBy, columnsToGroupBy, transformations, keyType
    );
    return query;
};

CampusExplorer.translateInputsToBuildQuery = (query, conditionType, conditionGroups, columnsToProject,
                                              sortOrderAndColumnToSortBy, columnsToGroupBy, transformations,
                                              keyType) => {
    let keysToType = {
        "dept": "string",
        "id": "string",
        "avg": "number",
        "instructor": "string",
        "title": "string",
        "pass": "number",
        "fail": "number",
        "audit": "number",
        "uuid": "string",
        "year": "number",
        "fullname": "string",
        "shortname": "string",
        "number": "string",
        "name": "string",
        "address": "string",
        "lat": "number",
        "lon": "number",
        "seats": "number",
        "type": "string",
        "furniture": "string",
        "href": "string"

    }
    if (conditionGroups.length === 0) {
        // do nothing
    } else if (conditionGroups.length === 1) {
        let obj;
        let value = conditionGroups[0].userInputTerm;
        if (conditionGroups[0].userInputTerm === "") {
            value = conditionGroups[0].userInputTerm;
        } else if (keysToType[conditionGroups[0].fieldSelected] === "number") {
            value = Number(conditionGroups[0].userInputTerm);
        } else if (keysToType[conditionGroups[0].fieldSelected] === "string") {
            value = String(conditionGroups[0].userInputTerm)
        }
        if (conditionGroups[0].isNotChecked === null) {
            obj = {
                [conditionGroups[0].operatorSelected]: {
                    [keyType + "_" + conditionGroups[0].fieldSelected]: value
                }
            }
        } else if (conditionGroups[0].isNotChecked === "checked") {
            obj = {
                NOT: {
                    [conditionGroups[0].operatorSelected]: {
                        [keyType + "_" + conditionGroups[0].fieldSelected]: value
                    }
                }
            }
        }
        query["WHERE"] = obj;
    } else if (conditionGroups.length > 1) {
        if (conditionType.all === true) {
            query["WHERE"] = {AND: []};
        } else if (conditionType.any === true) {
            query["WHERE"] = {OR: []};
        } else if (conditionType.none === true) {
            query["WHERE"] = {NOT: {OR: []}};
        }
        if (conditionGroups.length !== 0) {
            for (let condition of conditionGroups) {
                let obj;
                let value = condition.userInputTerm;
                if (condition.userInputTerm === "") {
                    value = condition.userInputTerm;
                } else if(keysToType[condition.fieldSelected] === "number") {
                    value = Number(condition.userInputTerm);
                } else if (keysToType[condition.fieldSelected] === "string") {
                    value = String(condition.userInputTerm)
                }
                if (condition.isNotChecked === null) {
                    obj = {
                        [condition.operatorSelected]: {
                            [keyType + "_" + condition.fieldSelected]: value
                        }
                    }
                } else if (condition.isNotChecked === "checked") {
                    obj = {
                        NOT: {
                            [condition.operatorSelected]: {
                                [keyType + "_" + condition.fieldSelected]: value
                            }
                        }
                    }
                }
                if (Object.keys(query["WHERE"]).toString() === "AND") {
                    query["WHERE"].AND.push(obj);
                } else if (Object.keys(query["WHERE"]).toString() === "OR") {
                    query["WHERE"].OR.push(obj);
                } else if (Object.keys(query["WHERE"]).toString() === "NOT") {
                    query["WHERE"].NOT.OR.push(obj);
                }
            }
        }
    }

    if (query["WHERE"] === undefined && (columnsToProject.length > 0 ||
        sortOrderAndColumnToSortBy.columnToSortBy.length > 0 ||
        columnsToGroupBy.length > 0 || transformations.length > 0)) {
        query["WHERE"] = {};
    }

    let addKeyTypeArr = ["dept", "id", "avg", "instructor", "title", "pass", "fail", "audit", "uuid", "year", "lat",
        "lon", "seats", "fullname", "shortname", "number", "name", "address", "type", "furniture", "href"];
    if (columnsToProject.length !== 0) {
        query["OPTIONS"] = {COLUMNS: []};
        for (let column of columnsToProject) {
            let name;
            let matched = false;
            for (let validName of addKeyTypeArr) {
                if (column === validName) {
                    name = keyType + "_" + column;
                    matched = true;
                }
            }
            if (!matched) {
                name = column;
            }
            query["OPTIONS"]["COLUMNS"].push(name);
        }
    }

    if (sortOrderAndColumnToSortBy.columnToSortBy.length !== 0) {
        if (query["OPTIONS"] === undefined) {
            query["OPTIONS"] = {};
        }

        let columnArr = [];
        if (sortOrderAndColumnToSortBy.columnToSortBy.length > 0) {
            for (let columnName of sortOrderAndColumnToSortBy.columnToSortBy) {
                let matched = false;
                for (let validName of addKeyTypeArr) {
                    if (columnName === validName) {
                        columnArr.push(keyType + "_" + columnName);
                        matched = true;
                    }
                }
                if (!matched) {
                    columnArr.push(columnName);
                }
            }
        }

        if (sortOrderAndColumnToSortBy.columnToSortBy.length === 1 &&
            sortOrderAndColumnToSortBy.sortOrder === null) {
            // query["OPTIONS"]["ORDER"] = sortOrderAndColumnToSortBy.columnToSortBy[0];
            query["OPTIONS"]["ORDER"] = columnArr[0];
        } else if (sortOrderAndColumnToSortBy.columnToSortBy.length === 1 &&
            sortOrderAndColumnToSortBy.sortOrder === "checked") {
            // query["OPTIONS"]["ORDER"] = {dir: "DOWN", keys: [sortOrderAndColumnToSortBy.columnToSortBy[0]]};
            query["OPTIONS"]["ORDER"] = {dir: "DOWN", keys: [columnArr[0]]};
        } else if (sortOrderAndColumnToSortBy.columnToSortBy.length > 1) {
            let order;
            if (sortOrderAndColumnToSortBy.sortOrder === "checked") {
                order = "DOWN";
            } else {
                order = "UP";
            }
            query["OPTIONS"]["ORDER"] = {dir: order, keys: []};
            // for (let order of sortOrderAndColumnToSortBy.columnToSortBy) {
            //     query["OPTIONS"]["ORDER"]["keys"].push(order);
            // }
            for (let order of columnArr) {
                query["OPTIONS"]["ORDER"]["keys"].push(order);
            }
        }
    }

    if (columnsToGroupBy.length !== 0) {
        query["TRANSFORMATIONS"] = {GROUP: []};
        for (let groupBy of columnsToGroupBy) {
            query["TRANSFORMATIONS"]["GROUP"].push(keyType + "_" + groupBy);
        }
    }
    if (transformations.length !== 0) {
        if (query["TRANSFORMATIONS"] === undefined) {
            query["TRANSFORMATIONS"] = {};
        }
        query["TRANSFORMATIONS"]["APPLY"] = [];
        for (let applyRule of transformations) {
            let obj = {[applyRule.userInputTerm]:
                    {[applyRule.operatorSelected]: keyType + "_" + applyRule.fieldSelected}};
            query["TRANSFORMATIONS"]["APPLY"].push(obj);
        }
    }

    // if (columnsToGroupBy.length !== 0 || transformations.length !== 0) {
    //     query["TRANSFORMATIONS"] = {GROUPS: [], APPLY: []};
    //     for (let groupBy of columnsToGroupBy) {
    //         query["TRANSFORMATIONS"]["GROUPS"].push(keyType + "_" + groupBy);
    //     }
    //
    //     for (let applyRule of transformations) {
    //         let obj = {[applyRule.userInputTerm]:
    //                 {[applyRule.operatorSelected]: keyType + "_" + applyRule.fieldSelected}};
    //         query["TRANSFORMATIONS"]["APPLY"].push(obj);
    //     }
    // }
}

CampusExplorer.getConditionType = (InsightDatasetKind) => {
    // one of the condition type attributes will be === "checked", otherwise they'll be null
    // all of the following == AND, any of the following == OR, none of the following == WHERE not
    let conditionType = {all: null, any: null, none: null};
    conditionType.all = document.getElementById(InsightDatasetKind + "-conditiontype-all").checked;
    conditionType.any = document.getElementById(InsightDatasetKind + "-conditiontype-any").checked;
    conditionType.none = document.getElementById(InsightDatasetKind + "-conditiontype-none").checked;
    return conditionType;
}

CampusExplorer.getConditionGroups = (activeTab) => {
    let conditionGroups = [];
    let controlGroupConditions = activeTab.getElementsByClassName("control-group condition");
    for (let controlGroupConditionRow of controlGroupConditions) {
        let conditionGroup = {isNotChecked: null, fieldSelected: null, operatorSelected: null, userInputTerm: null}
        // isNotChecked === "checked" if it's checked, null otherwise
        conditionGroup.isNotChecked = controlGroupConditionRow
            .getElementsByClassName("control not")[0]
            .querySelector("input")
            .getAttribute("checked");
        CampusExplorer.assignInputRow(controlGroupConditionRow, conditionGroup, conditionGroups);
    }
    return conditionGroups;
}

CampusExplorer.getTransformations = (activeTab) => {
    let transformations = []
    let transformationRows = activeTab.getElementsByClassName("transformations-container")[0].children
    for (let transformationRow of transformationRows) {
        let transformation = {userInputTerm: null, operatorSelected: null, fieldSelected: null}
        CampusExplorer.assignInputRow(transformationRow, transformation, transformations)
    }
    return transformations
}

CampusExplorer.assignInputRow = (row, inputAttrs, listOfInputAttrs) => {
    let userInputTerm = row
        .querySelector(".term")
        .querySelector('[value]');
    if (userInputTerm == null) {
        inputAttrs.userInputTerm = "";
    } else {
        inputAttrs.userInputTerm = userInputTerm.value
    }
    let operatorSelected = row
        .querySelector(".operators select")
        .querySelector('[selected]');
    // fieldSelected == "audit"
    let fieldSelected = row
        .querySelector(".fields select")
        .querySelector('[selected]');
    inputAttrs.operatorSelected = operatorSelected.value;
    inputAttrs.fieldSelected = fieldSelected.value;
    listOfInputAttrs.push(inputAttrs);
}

CampusExplorer.getColumnsToProject = (activeTab) => {
    let columnsToProject = [];
    let columnElements = activeTab
        .getElementsByClassName("form-group columns")[0]
        .getElementsByClassName("control-group")[0].children
    CampusExplorer.addColumnsToList(columnElements, columnsToProject)
    return columnsToProject;
}

CampusExplorer.getColumnsToGroupBy = (activeTab) => {
    let columnsToGroupBy = [];
    let columnElements = activeTab
        .getElementsByClassName("form-group groups")[0]
        .getElementsByClassName("control-group")[0].children
    CampusExplorer.addColumnsToList(columnElements, columnsToGroupBy)
    return columnsToGroupBy;
}

CampusExplorer.addColumnsToList = (columnElements, columns) => {
    for (let columnElement of columnElements) {
        if (columnElement.querySelector("input").checked) {
            let columnName = columnElement.querySelector("input").value;
            columns.push(columnName);
        }
    }
}

CampusExplorer.getSortOrderAndColumnToSortBy = (activeTab) => {
    let sortOrderAndColumnToSortBy = {sortOrder: null, columnToSortBy: []};
    let columnToSortBy = activeTab
        .getElementsByClassName("form-group order")[0]
        .getElementsByClassName("control order fields")[0]
        .querySelector(".fields select")
        .querySelectorAll('[selected]');
    columnToSortBy.forEach((column) => {
        sortOrderAndColumnToSortBy.columnToSortBy.push(column.value);
    })
    sortOrderAndColumnToSortBy.sortOrder = activeTab
        .getElementsByClassName("form-group order")[0]
        .getElementsByClassName("control descending")[0]
        .querySelector("input")
        .getAttribute("checked");
    return sortOrderAndColumnToSortBy;
}
