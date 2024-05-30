import {checkApplyKeyHelper, isObject} from "./PerformQueryHelper";

export function parseFormatCourses(section: any, id: string): any {
    let newDept = id + "_dept";
    let newID = id + "_id";
    let newAvg = id + "_avg";
    let newInstructor = id + "_instructor";
    let newTitle = id + "_title";
    let newPass = id + "_pass";
    let newFail = id + "_fail";
    let newAudit = id + "_audit";
    let newUuid = id + "_uuid";
    let newYear = id + "_year";
    let sectionFormatted = {
        [newDept]: section.dept,
        [newID]: section.id,
        [newAvg]: section.avg,
        [newInstructor]: section.instructor,
        [newTitle]: section.title,
        [newPass]: section.pass,
        [newFail]: section.fail,
        [newAudit]: section.audit,
        [newUuid]: section.uuid,
        [newYear]: section.year
    };
    return sectionFormatted;
}

export function parseFormatRooms(room: any, id: string): any {
    let newFullname = id + "_fullname";
    let newShortname = id + "_shortname";
    let newNumber = id + "_number";
    let newName = id + "_name";
    let newAddress = id + "_address";
    let newLat = id + "_lat";
    let newLon = id + "_lon";
    let newSeats = id + "_seats";
    let newType = id + "_type";
    let newFurniture = id + "_furniture";
    let newHref = id + "_href";
    let roomFormatted = {
        [newFullname]: room.fullname,
        [newShortname]: room.shortname,
        [newNumber]: room.number,
        [newName]: room.name,
        [newAddress]: room.address,
        [newLat]: room.lat,
        [newLon]: room.lon,
        [newSeats]: room.seats,
        [newType]: room.type,
        [newFurniture]: room.furniture,
        [newHref]: room.href
    };
    return roomFormatted;
}


export function createRequirementList(WHERE: any): any {
    if (WHERE.GT !== undefined) {
        let key = Object.keys(WHERE.GT).toString();
        return {operation: "GT", field: key.substring(key.indexOf("_") + 1, key.length),
            value: WHERE.GT[Object.keys(WHERE.GT)[0]]
        };
    }
    if (WHERE.LT !== undefined) {
        let key = Object.keys(WHERE.LT).toString();
        return {operation: "LT", field: key.substring(key.indexOf("_") + 1, key.length),
            value: WHERE.LT[Object.keys(WHERE.LT)[0]]
        };
    }
    if (WHERE.EQ !== undefined) {
        let key = Object.keys(WHERE.EQ).toString();
        return {operation: "EQ", field: key.substring(key.indexOf("_") + 1, key.length),
            value: WHERE.EQ[Object.keys(WHERE.EQ)[0]]
        };
    }
    if (WHERE.IS !== undefined) {
        let key = Object.keys(WHERE.IS).toString();
        return {operation: "IS", field: key.substring(key.indexOf("_") + 1, key.length),
            value: WHERE.IS[Object.keys(WHERE.IS)[0]]
        };
    }
    if (WHERE.NOT !== undefined) {
        let NOTObj = {operation: "NOT", object: {}};
        NOTObj.object = (createRequirementList(WHERE.NOT));
        return NOTObj;
    }
    if (WHERE.AND !== undefined) {
        let array: any = [];
        let ANDObj = {operation: "AND", ANDArray: array};
        for (let filter of WHERE.AND) {
            ANDObj.ANDArray.push(createRequirementList(filter));
        }
        return ANDObj;
    }
    if (WHERE.OR !== undefined) {
        let array: any = [];
        let ORObj = {operation: "OR", ORArray: array};
        for (let filter of WHERE.OR) {
            ORObj.ORArray.push(createRequirementList(filter));
        }
        return ORObj;
    }
}

function checkApplyTokenHelper(applyToken: string): boolean {
    let validTokens: string[] = ["MAX", "MIN", "AVG", "COUNT", "SUM"];
    let matched = false;
    for (let token of validTokens) {
        if (applyToken === token) {
            matched = true;
            break;
        }
    }
    if (!matched) {
        return false;
    }
    return true;
}

function checkApplyRule(applyRule: any): boolean {
    if (typeof applyRule !== "object") {
        return false;
    }
    if (Array.isArray(applyRule)) {
        return false;
    }
    if (Object.keys(applyRule).length !== 1) {
        return false;
    }
    if (!checkApplyKeyHelper(Object.keys(applyRule).toString())) {
        return false;
    }
    let applyKeyObj = applyRule[Object.keys(applyRule)[0]];
    if (applyKeyObj === undefined) {
        return false;
    }
    if (typeof applyKeyObj !== "object") {
        return false;
    }
    if (Array.isArray(applyKeyObj)) {
        return false;
    }
    if (Object.keys(applyKeyObj).length !== 1) {
        return false;
    }
    if (!checkApplyTokenHelper(Object.keys(applyKeyObj)[0])) {
        return false;
    }
    let applyTokenKey = applyKeyObj[Object.keys(applyKeyObj)[0]];
    if (typeof applyTokenKey !== "string") {
        return false;
    }
    if (!checkKeyHelper(applyTokenKey)) {
        return false;
    }
    return true;
}

export function checkTranformations(query: any): boolean {
    if (!("TRANSFORMATIONS" in query)) {
        return true;
    }
    if (!(isObject(query.TRANSFORMATIONS))) {
        return false;
    }
    if (Array.isArray(query.TRANSFORMATIONS)) {
        return false;
    }
    if (Object.keys(query.TRANSFORMATIONS).length !== 2) {
        return false;
    }
    if (query.TRANSFORMATIONS.GROUP == null || query.TRANSFORMATIONS.APPLY == null) {
        return false;
    }
    if (!Array.isArray(query.TRANSFORMATIONS.GROUP)) {
        return false;
    }
    if (query.TRANSFORMATIONS.GROUP.length < 1) {
        return false;
    }
    for (let key of query.TRANSFORMATIONS.GROUP) {
        if (typeof key !== "string") {
            return false;
        }
        if (!checkKeyHelper(key)) {
            return false;
        }
    }
    if (!Array.isArray(query.TRANSFORMATIONS.APPLY)) {
        return false;
    }
    for (let applyRule of query.TRANSFORMATIONS.APPLY) {
        if (!checkApplyRule(applyRule)) {
            return false;
        }
    }
    return true;
}

function checkAnyKeyHelper(key: string): boolean {
    let validFields: string[] = ["avg", "pass", "fail", "audit", "year", "lat", "lon", "seats",
        "dept", "id", "instructor", "title", "uuid", "fullname", "shortname", "number", "name", "address", "type",
        "furniture", "href"];
    let matchesKey = false;
    for (let field of validFields) {
        let validKeys = new RegExp("^[^_]+" + "_" + field);
        if (validKeys.test(key)) {
            matchesKey = true;
            break;
        }
    }
    if (!matchesKey) {
        if (key.trim().length === 0) {
            return false;
        }
        if (key.includes("_")) {
            return false;
        }
    }
    return true;
}

export function checkOptionsPartTwo(query: any): boolean {
    for (let queryKey of query.OPTIONS.COLUMNS) {
        if (typeof queryKey !== "string") {
            return false;
        }
        if (!checkAnyKeyHelper(queryKey)) {
            return false;
        }
    }
    if (query.OPTIONS.ORDER !== undefined) {
        if (typeof query.OPTIONS.ORDER === "string") {
            if (!checkAnyKeyHelper(query.OPTIONS.ORDER)) {
                return false;
            }
        } else if (typeof query.OPTIONS.ORDER === "object") {
            if (Array.isArray(query.OPTIONS.ORDER)) {
                return false;
            }
            if (Object.keys(query.OPTIONS.ORDER).length !== 2) {
                return false;
            }
            if (query.OPTIONS.ORDER.dir === undefined) {
                return false;
            }
            if (!(query.OPTIONS.ORDER.dir === "DOWN" || query.OPTIONS.ORDER.dir === "UP")) {
                return false;
            }
            if (query.OPTIONS.ORDER.keys === undefined) {
                return false;
            }
            if (!(Array.isArray(query.OPTIONS.ORDER.keys))) {
                return false;
            }
            if (query.OPTIONS.ORDER.keys.length === 0) {
                return false;
            }
            for (let anykeys of query.OPTIONS.ORDER.keys) {
                if (!checkAnyKeyHelper(anykeys)) {
                    return false;
                }
            }
        } else {
            return false;
        }
    }
    if (Object.keys(query.OPTIONS).length > 2) {
        return false;
    }
    return !(Object.keys(query.OPTIONS).length === 2 && query.OPTIONS.ORDER === undefined);
}

export function checkKeyHelper(key: string): boolean {
    let validFields: string[] = ["avg", "pass", "fail", "audit", "year", "lat", "lon", "seats",
        "dept", "id", "instructor", "title", "uuid", "fullname", "shortname", "number", "name", "address", "type",
        "furniture", "href"];
    let matched = false;
    for (let field of validFields) {
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
