import Decimal from "decimal.js";

export function applyGroupToData(result: any[], TRANSFORMATIONS: any): any[] {
    let groupingResultArray: any[] = [];
    let groupArray = TRANSFORMATIONS.GROUP;
    for (let sectionOrRoomObject of result) {
        let groupValueArray: any[] = [];
        for (let key of groupArray) {
            let groupValue = sectionOrRoomObject[key];
            groupValueArray.push(groupValue);
        }
        if (groupingResultArray.length === 0) {
            let newGroupObject: { group: any[]; resultQ: any[] };
            newGroupObject = {
                group: groupValueArray,
                resultQ: [sectionOrRoomObject]
            };
            groupingResultArray.push(newGroupObject);
        } else {
            let matched = false;
            for (let groupObject of groupingResultArray) {
                if (sameValuesInArrays(groupObject.group, groupValueArray)) {
                    groupObject.resultQ.push(sectionOrRoomObject);
                    matched = true;
                    break;
                }
            }
            if (!matched) {
                    let newGroupObject: { group: any[]; resultQ: any[] };
                    newGroupObject = {
                        group: [],
                        resultQ: []
                    };
                    newGroupObject.group = groupValueArray;
                    newGroupObject.resultQ.push(sectionOrRoomObject);
                    groupingResultArray.push(newGroupObject);
                }
            }
        }
    return groupingResultArray;
    }

function sameValuesInArrays(array1: any[], array2: any[]): boolean {
    if (array1.length !== array2.length) {
        return false;
    }
    for (let i = 0; i < array1.length; i++) {
        if (array1[i] !== array2[i]) {
            return false;
        }
    }
    return true;
}

export function applyApplyToData(result: any[], TRANSFORMATIONS: any): any[] {
    let applyRuleValueArray = [];
    for (let applyRule of TRANSFORMATIONS.APPLY) {
        let applyKeyObj = applyRule[Object.keys(applyRule)[0]];
        let applyToken = Object.keys(applyKeyObj).toString();
        let applyTokenKey = applyKeyObj[Object.keys(applyKeyObj)[0]];
        if (applyToken === "MAX") {
            let maxValueArray =  applyMaxHelper(result, applyTokenKey);
            applyRuleValueArray.push(maxValueArray);
        }
        if (applyToken === "MIN") {
            let minValueArray = applyMinHelper(result, applyTokenKey);
            applyRuleValueArray.push(minValueArray);
        }
        if (applyToken === "AVG") {
            let avgValueArray = applyAvgHelper(result, applyTokenKey);
            applyRuleValueArray.push(avgValueArray);
        }
        if (applyToken === "SUM") {
            let sumValueArray = applySumHelper(result, applyTokenKey);
            applyRuleValueArray.push(sumValueArray);
        }
        if (applyToken === "COUNT") {
            let countValueArray = applyCountHelper(result, applyTokenKey);
            applyRuleValueArray.push(countValueArray);
        }
    }
    return applyRuleValueArray;
}

function applyCountHelper(result: any[], applyTokenKey: string): any[] {
    let countValueArray = [];
    for (let groupObject of result) {
        let countSet = new Set();
        for (let queriedData of groupObject.resultQ) {
            countSet.add(queriedData[applyTokenKey]);
        }
        countValueArray.push(countSet.size);
    }
    return countValueArray;
}

function applySumHelper(result: any[], applyTokenKey: string): any[] {
    let sumValueArray = [];
    for (let groupObject of result) {
        let sumValue = 0;
        for (let queriedData of groupObject.resultQ) {
            sumValue = sumValue + queriedData[applyTokenKey];
        }
        sumValueArray.push(sumValue);
    }
    return sumValueArray;
}

function applyAvgHelper(result: any[], applyTokenKey: string): any[] {
    let avgValueArray = [];
    for (let groupObject of result) {
        let total = new Decimal(0);
        for (let queriedData of groupObject.resultQ) {
            let keyValue = new Decimal(queriedData[applyTokenKey]);
            total = Decimal.add(total, keyValue);
        }
        let avg = total.toNumber() / groupObject.resultQ.length;
        let res = Number(avg.toFixed(2));
        avgValueArray.push(res);
    }
    return avgValueArray;
}

function applyMinHelper(result: any[], applyTokenKey: string): any[] {
    let minValueArray = [];
    for (let groupObject of result) {
        let applyArray = [];
        for (let queriedData of groupObject.resultQ) {
            applyArray.push(queriedData[applyTokenKey]);
        }
        let minValue = Math.min(...applyArray);
        minValueArray.push(minValue);
    }
    return minValueArray;
}

function applyMaxHelper(result: any[], applyTokenKey: string): any[] {
    let maxValueArray = [];
    for (let groupObject of result) {
        let applyArray = [];
        for (let queriedData of groupObject.resultQ) {
            applyArray.push(queriedData[applyTokenKey]);
        }
        let maxValue = Math.max(...applyArray);
        maxValueArray.push(maxValue);
    }
    return maxValueArray;
}

export function onlyShowColumnsTransformation(query: any, result: any[], applyValueArray: any[]): any[] {
    let columnResultWithTransformations = [];
    let columns = query.OPTIONS.COLUMNS;
    for (let k = 0; k < result.length; k++) {
        let resultGroupedObject: any = {};
        for (let column of columns) {
            if (query.TRANSFORMATIONS.GROUP.includes(column)) {
                let indexNumber = query.TRANSFORMATIONS.GROUP.indexOf(column);
                let columnValue = result[k].group[indexNumber];
                let columnName: string = column;
                resultGroupedObject[columnName] = columnValue;
            } else {
                for (let j = 0; j < query.TRANSFORMATIONS.APPLY.length; j++) {
                    if (Object.keys(query.TRANSFORMATIONS.APPLY[j]).toString() === column) {
                        let columnValue = applyValueArray[j][k];
                        let columnName = column;
                        resultGroupedObject[columnName] = columnValue;
                    }
                }
            }
        }
        columnResultWithTransformations.push(resultGroupedObject);
    }
    return columnResultWithTransformations;
}

export function reorderResultTransformation(query: any, columnResultWithTransformations: any[]): any {
    if (query.OPTIONS.ORDER === undefined) {
        return columnResultWithTransformations;
    }
    if (typeof query.OPTIONS.ORDER === "string") {
        return reorderResult(query, columnResultWithTransformations);
    }
    if (typeof query.OPTIONS.ORDER === "object") {
        return reorderResultOrderComplex(query, columnResultWithTransformations);
    }
}

function reorderResultOrderComplex(query: any, columnResultWithTransformations: any[]): any {
    let array = columnResultWithTransformations;
    let firstOrderTarget = query.OPTIONS.ORDER.keys[0];
    let sortDirection = query.OPTIONS.ORDER.dir;
    if (sortDirection === "UP") {
        for (let i = 1; i < array.length; i++) {
            for (let j = i - 1; j > -1; j--) {
                // if there is a tie between two keys
                let matched = false;
                if (array[j + 1][firstOrderTarget] === array[j][firstOrderTarget]) {
                    for (let k = 1; k < query.OPTIONS.ORDER.keys.length; k++) {
                        if (array[j + 1][query.OPTIONS.ORDER.keys[k]] === array[j][query.OPTIONS.ORDER.keys[k]]) {
                            let doNothing;
                        } else if (array[j + 1][query.OPTIONS.ORDER.keys[k]] < array[j][query.OPTIONS.ORDER.keys[k]]) {
                            [array[j + 1], array[j]] = [array[j], array[j + 1]];
                            matched = true;
                            break;
                        } else if (array[j + 1][query.OPTIONS.ORDER.keys[k]] > array[j][query.OPTIONS.ORDER.keys[k]]) {
                            matched = true;
                            break;
                        }
                    }
                }
                if (!matched) {
                    if (array[j + 1][firstOrderTarget] < array[j][firstOrderTarget]) {
                        [array[j + 1], array[j]] = [array[j], array[j + 1]];
                    }
                }
            }
        }
        return array;
    }
    if (sortDirection === "DOWN") {
        for (let i = 1; i < array.length; i++) {
            for (let j = i - 1; j > -1; j--) {
                // if there is a tie between two keys
                if (array[j + 1][firstOrderTarget] === array[j][firstOrderTarget]) {
                    for (let k = 1; k < query.OPTIONS.ORDER.keys.length; k++) {
                        if (array[j + 1][query.OPTIONS.ORDER.keys[k]] > array[j][query.OPTIONS.ORDER.keys[k]]) {
                            [array[j + 1], array[j]] = [array[j], array[j + 1]];
                            break;
                        }
                    }
                }
                if (array[j + 1][firstOrderTarget] > array[j][firstOrderTarget]) {
                    [array[j + 1], array[j]] = [array[j], array[j + 1]];
                }
            }
        }
        return array;
    }
}


export function onlyShowColumns(query: any, result: any, id: string): any {
    let columns = query.OPTIONS.COLUMNS;
    let columnsField: string[] = [];
    for (let column of columns) {
        columnsField.push(column);
    }
    let validColumns: string[] = [id + "_dept", id + "_id", id + "_avg", id + "_instructor", id + "_title",
        id + "_pass", id + "_fail", id + "_audit", id + "_uuid", id + "_year", id + "_lat", id + "_lon",
        id + "_seats", id + "_fullname", id + "_shortname", id + "_number", id + "_name", id + "_address",
        id + "_type", id + "_furniture", id + "_href"];
    let difference = validColumns.filter((x) => !columnsField.includes(x));
    for (let field of difference) {
        for (let section of result) {
            if (section.hasOwnProperty(field)) {
                delete section[field];
            }
        }
    }
    return result;
}

export function reorderResultNoTransformation(query: any, result: any): any {
    if (query.OPTIONS.ORDER === undefined) {
        return result;
    }
    if (typeof query.OPTIONS.ORDER === "string") {
        return reorderResult(query, result);
    }
    if (typeof query.OPTIONS.ORDER === "object") {
        return reorderResultOrderComplex(query, result);
    }
}

export function reorderResult(query: any, result: any): any {
    if (query.OPTIONS.ORDER === undefined) {
        return result;
    }
    let orderTarget = query.OPTIONS.ORDER;
    return result.sort((section1: any, section2: any) => (section1[orderTarget] > section2[orderTarget]) ? 1 : -1);
}
