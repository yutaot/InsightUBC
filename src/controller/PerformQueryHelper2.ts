import {checkKeyHelper} from "./PerformQueryHelper4";
import {checkApplyKeyHelper} from "./PerformQueryHelper";

export function checkSemanticsOfQuery(query: any): boolean {
    if (checkApplyKeyInApplyRuleShouldBeUnique(query) === false) {
        return false;
    }
    if (checkColumnAnyKeysAreInGroupOrApply(query) === false) {
        return false;
    }
    if (checkSortKeyInColumns(query) === false) {
        return false;
    }
    if (checkApplyTokenValue(query) === false) {
        return false;
    }
    if (this.checkReferenceOnlyOneDataset(query) === false) {
        return false;
    }
    return true;
}

function checkApplyTokenValue(query: any): boolean {
    if (query.TRANSFORMATIONS === undefined) {
        return true;
    }
    for (let applyRule of query.TRANSFORMATIONS.APPLY) {
        let applyKeyObj = applyRule[Object.keys(applyRule)[0]];
        let applyToken = Object.keys(applyKeyObj).toString();
        let applyTokenKey = applyKeyObj[Object.keys(applyKeyObj)[0]];
        if (applyToken === "MAX" || applyToken === "MIN" || applyToken === "AVG" || applyToken === "SUM") {
            if (checkIfNumericKey(applyTokenKey) === false) {
                return false;
            }
        }
        if (applyToken === "COUNT") {
            if (checkKeyHelper(applyTokenKey) === false) {
                return false;
            }
        }
    }
    return true;
}

function checkIfNumericKey(key: string): boolean {
    let validNumKeys = ["avg", "pass", "fail", "audit", "year", "lat", "lon", "seats"];
    let matched = false;
    for (let field of validNumKeys) {
        let validKeys = new RegExp("^[^_]+" + "_" + field);
        if (validKeys.test(key)) {
            matched = true;
            break;
        }
    }
    if (!matched) {
        return false;
    }
    return true;
}

function checkSortKeyInColumns(query: any): boolean {
    if (query.OPTIONS.ORDER !== undefined) {
        if (typeof query.OPTIONS.ORDER === "string") {
            let orderKey = query.OPTIONS.ORDER;
            let exists = false;
            for (let anyKey of query.OPTIONS.COLUMNS) {
                if (anyKey === orderKey) {
                    exists = true;
                    break;
                }
            }
            if (!exists) {
                return false;
            }
        }
        if (typeof query.OPTIONS.ORDER === "object") {
            let orderKeyArray = query.OPTIONS.ORDER.keys;
            for (let key of orderKeyArray) {
                let exists = false;
                for (let anyKey of query.OPTIONS.COLUMNS) {
                    if (anyKey === key) {
                        exists = true;
                        break;
                    }
                }
                if (!exists) {
                    return false;
                }
            }
        }
    }
    return true;
}

function checkColumnAnyKeysAreInGroupOrApply(query: any): boolean {
    if (query.TRANSFORMATIONS === undefined) {
        for (let columnKey of query.OPTIONS.COLUMNS) {
            if (checkKeyHelper(columnKey) === false && checkApplyKeyHelper(columnKey) === true) {
                return false;
            }
        }
        return true;
    }
    for (let columnKey of query.OPTIONS.COLUMNS) {
        let matched = false;
        for (let key of query.TRANSFORMATIONS.GROUP) {
            if (columnKey === key) {
                matched = true;
                break;
            }
        }
        for (let applyKey of query.TRANSFORMATIONS.APPLY) {
            let applyKeyName = Object.keys(applyKey).toString();
            if (columnKey === applyKeyName) {
                matched = true;
                break;
            }
        }
        if (!matched) {
            return false;
        }
    }
    return true;
}

function checkApplyKeyInApplyRuleShouldBeUnique(query: any): boolean {
    if (query.TRANSFORMATIONS === undefined) {
        return true;
    }
    let applyKeysArray = [];
    for (let applyRule of query.TRANSFORMATIONS.APPLY) {
        let applyKey = Object.keys(applyRule).toString();
        applyKeysArray.push(applyKey);
    }
    let ApplyKeySet = new Set(applyKeysArray);
    if (ApplyKeySet.size !== applyKeysArray.length) {
        return false;
    }
    return true;
}

export function checkReferenceOnlyOneDataset(query: any): boolean {
    if (this.checkColumnsPartReferenceOneDataset(query.OPTIONS.COLUMNS) === false) {
        return false;
    }
    if (query.OPTIONS.ORDER !== undefined) {
        if (checkOrderPartReferenceOneDataset(query.OPTIONS.ORDER) === false) {
            return false;
        }
    }
    if (query.TRANSFORMATIONS !== undefined) {
        if (checkGroupPartReferenceOneDataset(query.TRANSFORMATIONS.GROUP) === false) {
            return false;
        }
        if (checkApplyPartReferenceOneDataset(query.TRANSFORMATIONS.APPLY) === false) {
            return false;
        }
    }
    if (this.checkWHEREPartReferenceOneDataset(query.WHERE) === false) {
        return false;
    }
    return true;
}

export let idWhere: string = null;

function checkGroupPartReferenceOneDataset(GROUP: any): boolean {
    if (idWhere === "") {
        idWhere = GROUP[0].substring(0, GROUP[0].indexOf("_"));
    }
    for (let key of GROUP) {
        if (key.substring(0, key.indexOf("_")) !== idWhere) {
            return false;
        }
    }
    return true;
}

function checkApplyPartReferenceOneDataset(APPLY: any): boolean {
    for (let applyRule of APPLY) {
        let applyKeyObj = applyRule[Object.keys(applyRule)[0]];
        let applyTokenKey = applyKeyObj[Object.keys(applyKeyObj)[0]];
        if (applyTokenKey.substring(0, applyTokenKey.indexOf("_")) !== idWhere) {
            return false;
        }
    }
    return true;
}

function checkOrderPartReferenceOneDataset(ORDER: any): boolean {
    if (typeof ORDER === "string") {
        if (ORDER.substring(0, ORDER.indexOf("_")).length === 0) {
            return true;
        } else if (ORDER.substring(0, ORDER.indexOf("_")) !== idWhere) {
            return false;
        }
    }
    if (typeof ORDER === "object") {
        for (let key of ORDER.keys) {
            if (key.substring(0, key.indexOf("_")).length === 0) {
                let doNothing;
            } else if (key.substring(0, key.indexOf("_")) !== idWhere) {
                return false;
            }
        }
    }
    return true;
}

function checkWHEREPartOneReference(WHERE: any): boolean {
    if (WHERE !== undefined) {
        let id = Object.keys(WHERE).toString().substring(0, Object.keys(WHERE).toString().indexOf("_"));
        if (idWhere === null || idWhere === "") {
            idWhere = id;
        } else if (idWhere !== id) {
            return false;
        }
    }
    return true;
}

export function checkWHEREPartReferenceOneDataset(WHERE: any): boolean {
    if (!checkWHEREPartOneReference(WHERE.GT)) {
        return false;
    }
    if (!checkWHEREPartOneReference(WHERE.LT)) {
        return false;
    }
    if (!checkWHEREPartOneReference(WHERE.EQ)) {
        return false;
    }
    if (!checkWHEREPartOneReference(WHERE.IS)) {
        return false;
    }
    if (WHERE.NOT !== undefined) {
        if (checkWHEREPartReferenceOneDataset(WHERE.NOT) === false) {
            return false;
        }
    }
    if (WHERE.AND !== undefined) {
        for (let filter of WHERE.AND) {
            if (this.checkWHEREPartReferenceOneDataset(filter) === false) {
                return false;
            }
        }
    }
    if (WHERE.OR !== undefined) {
        for (let filter of WHERE.OR) {
            if (this.checkWHEREPartReferenceOneDataset(filter) === false) {
                return false;
            }
        }
    }
    return true;
}

export function checkColumnsPartReferenceOneDataset(COLUMNS: any): boolean {
    let origID = "";
    for (let key of COLUMNS) {
        if (key.substring(0, key.indexOf("_")).length > 0) {
            origID = key.substring(0, key.indexOf("_"));
            break;
        }
    }
    // let origId = COLUMNS[0].substring(0, COLUMNS[0].indexOf("_"));
    for (let key of COLUMNS) {
        let id = key.substring(0, key.indexOf("_"));
        if (id !== origID && id.length !== 0) {
            return false;
        }
    }
    // origID may be empty string or "rooms"
    idWhere = origID;
    return true;
}

