// TODO: add test cases for query not json
import {checkOptionsPartTwo, checkTranformations} from "./PerformQueryHelper4";

function checkExcessKeys(query: any): boolean {
        if (!(Object.keys(query).length === 2 || Object.keys(query).length === 3)) {
            return false;
        }
        if (Object.keys(query).length === 2) {
            if (query.WHERE === undefined) {
                return false;
            }
            if (query.OPTIONS === undefined) {
                return false;
            }
        }
        if (Object.keys(query).length === 3) {
            if (query.WHERE === undefined) {
                return false;
            }
            if (query.OPTIONS === undefined) {
                return false;
            }
            if (query.TRANSFORMATIONS === undefined) {
                return false;
            }
        }
        if (query.WHERE == null) {
            return false;
        }
        if (query.WHERE.constructor !== Object) {
            return false;
        }
        return true;
    }

function checkOptionsPartOne(query: any): boolean {
        if (query.OPTIONS === undefined) {
            return false;
        }
        if (query.OPTIONS.constructor !== Object) {
            return false;
        }
        if (query.OPTIONS.COLUMNS === undefined) {
            return false;
        }
        return !(!(Array.isArray(query.OPTIONS.COLUMNS)) || query.OPTIONS.COLUMNS.length === 0);
}

export function checkApplyKeyHelper(key: string): boolean {
    if (key.trim().length === 0) {
        return false;
    }
    if (key.includes("_")) {
        return false;
    }
    return true;
}

export function checkValidityOfQuery(query: any): boolean {
        if (!(isObject(query))) {
            return false;
        }
        try {
            if (query.constructor.name !== "Object") {
                return false;
            }
        } catch (e) {
            return false;
        }
        if (!checkExcessKeys(query)) {
            return false;
        }
        if (Object.keys(query.WHERE).length === 0 && query.WHERE.constructor === Object) {
            let a = "im suffering";
        } else if (Object.keys(query.WHERE).length > 1) {
            return false;
        } else if (checkWhereValidity(query.WHERE) === false) {
            return false;
        }
        if (!checkOptionsPartOne(query)) {
            return false;
        }
        if (!checkOptionsPartTwo(query)) {
            return false;
        }
        if (!checkTranformations(query)) {
            return false;
        }
        return true;
}

export function checkGTHelper(WHERE: any, validFieldsMKeys: string[]): boolean {
        if (WHERE.GT == null) {
            return false;
        }
        if (Object.keys(WHERE.GT).length !== 1) {
            return false;
        }
        if (WHERE.GT.constructor !== Object) {
            return false;
        }
        if (Object.keys(WHERE.GT).length !== 1) {
            return false;
        }
        let GTKey = Object.keys(WHERE.GT).toString();
        let matched = false;
        for (let field of validFieldsMKeys) {
            let validKeys = new RegExp("^[^_]+" + "_" + field);
            if (validKeys.test(GTKey)) {
                matched = true;
                break;
            }
        }
        if (!matched) {
            return false;
        }
        if (typeof WHERE.GT[Object.keys(WHERE.GT)[0]] !== "number") {
            return false;
        }
}

export function checkLTHelper(WHERE: any, validFieldsMKeys: string[]): boolean {
        if (WHERE.LT == null) {
            return false;
        }
        if (Object.keys(WHERE.LT).length !== 1) {
            return false;
        }
        if (WHERE.LT.constructor !== Object) {
            return false;
        }
        if (Object.keys(WHERE.LT).length !== 1) {
            return false;
        }
        let LTKey = Object.keys(WHERE.LT).toString();
        let matched = false;
        for (let field of validFieldsMKeys) {
            let validKeys = new RegExp("^[^_]+" + "_" + field);
            if (validKeys.test(LTKey)) {
                matched = true;
                break;
            }
        }
        if (!matched) {
            return false;
        }
        if (typeof WHERE.LT[Object.keys(WHERE.LT)[0]] !== "number") {
            return false;
        }
}

export function checkEQHelper(WHERE: any, validFieldsMKeys: string[]): boolean {
        if (WHERE.EQ == null) {
            return false;
        }
        if (Object.keys(WHERE.EQ).length !== 1) {
            return false;
        }
        if (WHERE.EQ.constructor !== Object) {
            return false;
        }
        if (Object.keys(WHERE.EQ).length !== 1) {
            return false;
        }
        let EQKey = Object.keys(WHERE.EQ).toString();
        let matched = false;
        for (let field of validFieldsMKeys) {
            let validKeys = new RegExp("^[^_]+" + "_" + field);
            if (validKeys.test(EQKey)) {
                matched = true;
                break;
            }
        }
        if (!matched) {
            return false;
        }
        if (typeof WHERE.EQ[Object.keys(WHERE.EQ)[0]] !== "number") {
            return false;
        }
}

function checkIsValidity(WHERE: any): boolean {
        if (WHERE.IS == null) {
            return false;
        }
        if (WHERE.IS.constructor !== Object) {
            return false;
        }
        if (Object.keys(WHERE.IS).length !== 1) {
            return false;
        }
        let validFieldsSKey: string[] = ["dept", "id", "instructor", "title", "uuid", "fullname", "shortname",
        "number", "name", "address", "type", "furniture", "href"];
        let ISKey = Object.keys(WHERE.IS).toString();
        let matched = false;
        for (let field of validFieldsSKey) {
            let validKeys = new RegExp("^[^_]+" + "_" + field);
            if (validKeys.test(ISKey)) {
                matched = true;
                break;
            }
        }
        if (!matched) {
            return false;
        }
        if (typeof WHERE.IS[Object.keys(WHERE.IS)[0]] !== "string") {
            return false;
        }
        let inputString = WHERE.IS[Object.keys(WHERE.IS)[0]];
        if (inputString === "*" || inputString === "**") {
            let doNothing;
        } else if (inputString.substring(1, inputString.length - 1).includes("*")) {
            return false;
        }
        return true;
}

function checkANDValidity(WHERE: any): boolean {
        if (WHERE.AND == null) {
            return false;
        }
        if (!(Array.isArray(WHERE.AND))) {
            return false;
        }
        if (WHERE.AND.length === 0) {
            return false;
        }
        for (let filter of WHERE.AND) {
            if (checkWhereValidity(filter) === false) {
                return false;
            }
        }
        return true;
}

function checkORValidity(WHERE: any): boolean {
        if (WHERE.OR == null) {
            return false;
        }
        if (!(Array.isArray(WHERE.OR))) {
            return false;
        }
        if (WHERE.OR.length === 0) {
            return false;
        }
        for (let filter of WHERE.OR) {
            if (checkWhereValidity(filter) === false) {
                return false;
            }
        }
        return true;
}

export function isObject(varToCheck: any): boolean {
    if (!(varToCheck !== null && typeof varToCheck === "object")) {
        return false;
    }
    if (varToCheck.constructor.name !== "Object") {
        return false;
    }
    return true;
}

export function checkWhereValidity(WHERE: any): boolean {
        let validFieldsMKeys: string[] = ["avg", "pass", "fail", "audit", "year", "lat", "lon", "seats"];
        if (!(isObject(WHERE))) {
            return false;
        }
        if (Object.keys(WHERE).length > 1) {
            return false;
        }
        if (WHERE.GT !== undefined) {
            return checkGTHelper(WHERE, validFieldsMKeys);
        } else if (WHERE.LT !== undefined) {
            return checkLTHelper(WHERE, validFieldsMKeys);
        } else if (WHERE.EQ !== undefined) {
            return checkEQHelper(WHERE, validFieldsMKeys);
        } else if (WHERE.NOT !== undefined) {
            if (!(isObject(WHERE.NOT))) {
                return false;
            }
            return checkWhereValidity(WHERE.NOT);
        } else if (WHERE.IS !== undefined) {
            if (!checkIsValidity(WHERE)) {
                return false;
            }
        } else if (WHERE.AND !== undefined) {
            return checkANDValidity(WHERE);
        } else if (WHERE.OR !== undefined) {
            return checkORValidity(WHERE);
        } else {
            return false;
        }
    }

