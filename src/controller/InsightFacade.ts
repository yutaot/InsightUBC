import Log from "../Util";
import {
    CourseSection, IInsightFacade, InsightDataset, InsightDatasetKind, InsightError, NotFoundError, ResultTooLargeError,
    Room
} from "./IInsightFacade";
import * as fs from "fs";
import {validateContent, validateId, validateKind, } from "./AddRemoveListDatasetsHelper";
import StoreCoursesDataset from "./StoreCoursesDataset";
import StoreRoomsDataset from "./StoreRoomsDataset";
import {checkValidityOfQuery} from "./PerformQueryHelper";
import {checkSemanticsOfQuery, idWhere} from "./PerformQueryHelper2";
import {
    applyApplyToData,
    applyGroupToData,
    onlyShowColumns,
    onlyShowColumnsTransformation,
    reorderResult,
    reorderResultNoTransformation,
    reorderResultTransformation
} from "./PerformQueryHelper3";
import {createRequirementList, parseFormatCourses, parseFormatRooms} from "./PerformQueryHelper4";

/**
 * This is the main programmatic entry point for the project.
 * Method documentation is in IInsightFacade
 *
 */
export default class InsightFacade implements IInsightFacade {
    private coursesDatasets: {
        [id: string]: {
            sections: CourseSection[],
            kind: InsightDatasetKind
        }
    };

    private roomsDatasets: {
        [id: string]: {
            rooms: Room[],
            kind: InsightDatasetKind
        }
    };

    // TODO: use dirname
    private coursesDatasetsPath = __dirname + "/../../data/coursesDatasets.json";
    private roomsDatasetsPath = __dirname + "/../../data/roomsDatasets.json";

    constructor() {
        Log.trace("InsightFacadeImpl::init()");
        try {
            if (fs.existsSync(this.coursesDatasetsPath)) {
                let coursesDatasetsInString = fs.readFileSync(this.coursesDatasetsPath, "utf8");
                this.coursesDatasets = JSON.parse(coursesDatasetsInString);
            } else {
                this.coursesDatasets = {};
            }
        } catch (e) {
            this.coursesDatasets = {};
        }

        try {
            if (fs.existsSync(this.roomsDatasetsPath)) {
                let roomsDatasetsInString = fs.readFileSync(this.roomsDatasetsPath, "utf8");
                this.roomsDatasets = JSON.parse(roomsDatasetsInString);
            } else {
                this.roomsDatasets = {};
            }
        } catch (e) {
            this.roomsDatasets = {};
        }
    }

    public addDataset(id: string, content: string, kind: InsightDatasetKind): Promise<string[]> {
        if (!validateId(id)) {
            return Promise.reject(new InsightError());
        }
        if (kind === InsightDatasetKind.Courses && id in this.coursesDatasets) {
            return Promise.reject(new InsightError());
        }
        if (kind === InsightDatasetKind.Rooms && id in this.roomsDatasets) {
            return Promise.reject(new InsightError());
        }
        if (!validateContent(content)) {
            return Promise.reject(new InsightError());
        }
        if (!validateKind(kind)) {
            return Promise.reject(new InsightError());
        }
        if (kind === InsightDatasetKind.Rooms) {
            let storeRoomsDataset: StoreRoomsDataset = new StoreRoomsDataset();
            return storeRoomsDataset.storeDatasetToMemoryAndDisk(
                id, content, kind, this.roomsDatasets, this.coursesDatasets, this.roomsDatasetsPath
            );
        } else if (kind === InsightDatasetKind.Courses) {
            let storeCoursesDataset: StoreCoursesDataset = new StoreCoursesDataset();
            return storeCoursesDataset.storeDatasetToMemoryAndDisk(
                id, content, kind, this.coursesDatasets, this.roomsDatasets, this.coursesDatasetsPath
            );
        } else {
            return Promise.reject(new InsightError());
        }
    }

    public removeDataset(id: string): Promise<string> {
        if (!validateId(id)) {
            return Promise.reject(new InsightError());
        }
        if (!(id in this.coursesDatasets) && !(id in this.roomsDatasets)) {
            return Promise.reject(new NotFoundError());
        }
        if (id in this.coursesDatasets) {
            delete this.coursesDatasets[id];
            fs.writeFileSync(
                this.coursesDatasetsPath,
                JSON.stringify(this.coursesDatasets, null, 2),
                "utf-8"
            );
            return Promise.resolve(id);
        }
        if (id in this.roomsDatasets) {
            delete this.roomsDatasets[id];
            fs.writeFileSync(
                this.roomsDatasetsPath,
                JSON.stringify(this.roomsDatasets, null, 2),
                "utf-8"
            );
            return Promise.resolve(id);
        }
    }

    public performQuery(query: any): Promise<any[]> {
        if (checkValidityOfQuery(query) === false) {
            return Promise.reject(new InsightError("error is in checkValidityOfQuery"));
        }
        if (checkSemanticsOfQuery(query) === false) {
            return Promise.reject(new InsightError("error is in checkSemanticsOfQuery"));
        }
        // checks if queried id exists in dataset
        // TODO: for now, I will assume idWhere contains dataset reference by the end of semantic checking
        let datasetKind;
        let id = idWhere;
        let matched = false;
        for (let uniqueID in this.coursesDatasets) {
            let uID = uniqueID.toString();
            if (id === uID) {
                matched = true;
                datasetKind = "courses";
                break;
            }
        }
        for (let uniqueID in this.roomsDatasets) {
            let uID = uniqueID.toString();
            if (id === uID) {
                matched = true;
                datasetKind = "rooms";
                break;
            }
        }
        if (!matched) {
            return Promise.reject(new InsightError("error is in check if queried id exists in dataset"));
        }
        let requirementObject = createRequirementList(query.WHERE);
        let result: any[] = [];
        result = this.getDataFromCorrespondingDataset(result, requirementObject, id, datasetKind);
        let reorderedResult;
        if (query.TRANSFORMATIONS !== undefined) {
            result = applyGroupToData(result, query.TRANSFORMATIONS);
            let applyValueArray = applyApplyToData(result, query.TRANSFORMATIONS);
            let columnResultWithTransformations = onlyShowColumnsTransformation(query, result, applyValueArray);
            reorderedResult = reorderResultTransformation(query, columnResultWithTransformations);
        } else {
            let columnsResult = onlyShowColumns(query, result, id);
            reorderedResult = reorderResultNoTransformation(query, columnsResult);
        }
        if (result.length > 5000) {
            return Promise.reject(new ResultTooLargeError());
        }
        return Promise.resolve(reorderedResult);
    }

    public getDataFromCorrespondingDataset(result: any[], requirementObject: any,
                                           id: string, datasetKind: string): any[] {
        if (requirementObject !== undefined) {
            if (datasetKind === "courses") {
                for (let section of this.coursesDatasets[id].sections) {
                    if (this.requirementsPassed(section, requirementObject)) {
                        let parsedSection = parseFormatCourses(section, id);
                        result.push(parsedSection);
                    }
                }
                return result;
            }
            if (datasetKind === "rooms") {
                for (let room of this.roomsDatasets[id].rooms) {
                    if (this.requirementsPassed(room, requirementObject)) {
                        let parsedSection = parseFormatRooms(room, id);
                        result.push(parsedSection);
                    }
                }
                return result;
            }
        } else {
            if (datasetKind === "courses") {
                for (let section of this.coursesDatasets[id].sections) {
                    let parsedSection = parseFormatCourses(section, id);
                    result.push(parsedSection);
                }
                return result;
            }
            if (datasetKind === "rooms") {
                for (let room of this.roomsDatasets[id].rooms) {
                    let parsedSection = parseFormatRooms(room, id);
                    result.push(parsedSection);
                }
                return result;
            }
        }
    }

    public requirementsPassed(section: any, reqObj: any): boolean {
        let queryValue = reqObj.value;
        let sectionValue = section[reqObj.field];
        if (reqObj.operation === "GT") {
            return sectionValue > queryValue;
        }
        if (reqObj.operation === "LT") {
            return sectionValue < queryValue;
        }
        if (reqObj.operation === "EQ") {
            return sectionValue === queryValue;
        }
        if (reqObj.operation === "IS") {
            return this.validateAsterisksInQuery(queryValue, sectionValue);
        }
        if (reqObj.operation === "NOT") {
            return !this.requirementsPassed(section, reqObj.object);
        }
        if (reqObj.operation === "AND") {
            for (let object of reqObj.ANDArray) {
                if (!this.requirementsPassed(section, object)) {
                    return false;
                }
            }
            return true;
        }
        if (reqObj.operation === "OR") {
            for (let object of reqObj.ORArray) {
                if (this.requirementsPassed(section, object)) {
                    return true;
                }
            }
            return false;
        }
    }

    private validateAsterisksInQuery(queryValue: any, sectionValue: any): boolean {
        if (queryValue === "*") {
            return true;
        }
        if (queryValue === "**") {
            return true;
        }
        if (queryValue.startsWith("*") && queryValue.endsWith("*")) {
            return sectionValue.includes(queryValue.substring(1, queryValue.length - 1));
        }
        if (queryValue.startsWith("*")) {
            return sectionValue.endsWith(queryValue.substring(1));
        }
        if (queryValue.endsWith("*")) {
            return sectionValue.startsWith(queryValue.substring(0, queryValue.length - 1));
        }
        if (!queryValue.includes("*")) {
            return sectionValue === queryValue;
        }
    }

    public listDatasets(): Promise<InsightDataset[]> {
        let allCurrentlyAddedDatasets: InsightDataset[] = [];
        for (let id in this.coursesDatasets) {
            let sectionsAndKind = this.coursesDatasets[id];
            let insightDataset: InsightDataset = {
                id: id,
                kind: sectionsAndKind.kind,
                numRows: sectionsAndKind.sections.length
            };
            allCurrentlyAddedDatasets.push(insightDataset);
        }
        for (let id in this.roomsDatasets) {
            let roomsAndKind = this.roomsDatasets[id];
            let insightDataset: InsightDataset = {
                id: id,
                kind: roomsAndKind.kind,
                numRows: roomsAndKind.rooms.length
            };
            allCurrentlyAddedDatasets.push(insightDataset);
        }
        return Promise.resolve(allCurrentlyAddedDatasets);
    }
}
