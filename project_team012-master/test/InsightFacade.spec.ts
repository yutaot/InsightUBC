import * as chai from "chai";
import {expect} from "chai";
import * as fs from "fs-extra";
import * as chaiAsPromised from "chai-as-promised";
import {InsightDataset, InsightDatasetKind, InsightError, NotFoundError} from "../src/controller/IInsightFacade";
import InsightFacade from "../src/controller/InsightFacade";
import Log from "../src/Util";
import TestUtil from "./TestUtil";

// This should match the schema given to TestUtil.validate(..) in TestUtil.readTestQueries(..)
// except 'filename' which is injected when the file is read.
export interface ITestQuery {
    title: string;
    query: any; // make any to allow testing structurally invalid queries
    isQueryValid: boolean;
    result: any;
    filename: string; // This is injected when reading the file
}

describe("InsightFacade Add/Remove/List Dataset", function () {
    // Reference any datasets you've added to test/data here and they will
    // automatically be loaded in the 'before' hook.
    const datasetsToLoad: { [id: string]: string } = {
        courses: "./test/data/courses.zip",
        zipFileWithNoCourses: "./test/data/zipFileWIthNoCourses.zip",             // Invalid
        // coursesUnzipped: "./test/data/coursesUnzipped.zip",                           // Invalid
        JsonFile: "./test/data/CPSC310.zip",            // Invalid
        coursesWithNoValidCourseSection: "./test/data/coursesWithNoValidCourseSection.zip", // Invalid
        coursesWithValidCourseSections: "./test/data/coursesWithValidCourseSections.zip", // Valid
        coursesWithNoValidJson: "./test/data/coursesWithNoValidJson.zip", // Invalid
        coursesEmpty: "./test/data/coursesEmpty.zip", // Invalid
        hello: "./test/data/hello.zip", // Invalid
        123: "./test/data/123.zip", // Valid
        rooms: "./test/data/rooms.zip", // Valid
        roomsNotInHtml: "./test/data/roomsNotInHtml.zip", // Invalid
        roomsWithoutAnyRoom: "./test/data/roomsWithoutAnyRoom.zip", // Invalid
        roomsWithValidRoom: "./test/data/roomsWithValidRoom.zip", // Valid
        roomsWithRootFolderNamedUbc: "./test/data/roomsWithRootFolderNamedUbc.zip", // Invalid
        roomsWithNoIndex: "./test/data/roomsWithNoIndex.zip"
    };
    let datasets: { [id: string]: string } = {};
    let insightFacade: InsightFacade;
    const cacheDir = __dirname + "/../data";

    before(function () {
        // This section runs once and loads all datasets specified in the datasetsToLoad object
        // into the datasets object
        Log.test(`Before all`);
        chai.use(chaiAsPromised);
        if (!fs.existsSync(cacheDir)) {
            fs.mkdirSync(cacheDir);
        }
        for (const id of Object.keys(datasetsToLoad)) {
            datasets[id] = fs
                .readFileSync(datasetsToLoad[id])
                .toString("base64");
        }
        try {
            insightFacade = new InsightFacade();
        } catch (err) {
            Log.error(err);
        }
    });

    beforeEach(function () {
        Log.test(`BeforeTest: ${this.currentTest.title}`);
    });

    after(function () {
        Log.test(`After: ${this.test.parent.title}`);
    });

    afterEach(function () {
        // This section resets the data directory (removing any cached data) and resets the InsightFacade instance
        // This runs after each test, which should make each test independent from the previous one
        Log.test(`AfterTest: ${this.currentTest.title}`);
        try {
            fs.removeSync(cacheDir);
            fs.mkdirSync(cacheDir);
            insightFacade = new InsightFacade();
        } catch (err) {
            Log.error(err);
        }
    });

    // This is a unit test. You should create more like this!
    it("Should add a valid dataset", function () {
        const id: string = "coursesWithValidCourseSections";
        const expected: string[] = [id];
        const futureResult: Promise<string[]> = insightFacade.addDataset(
            id,
            datasets[id],
            InsightDatasetKind.Courses,
        );
        return expect(futureResult).to.eventually.deep.equal(expected);
    });

    // Test addDataset, value and type check id
    it("test addDataset, given empty string for id raise value error", function () {
        const invalidId: string = "";
        const validId: string = "courses";
        const futureResult: Promise<string[]> = insightFacade.addDataset(
            invalidId,
            datasets[validId],
            InsightDatasetKind.Courses
        );
        return expect(futureResult).to.be.rejectedWith(InsightError);
    });

    it("test addDataset, given undefined for id raise InsightError", function () {
        const invalidId: string = undefined;
        const validId: string = "courses";
        const futureResult: Promise<string[]> = insightFacade.addDataset(
            invalidId,
            datasets[validId],
            InsightDatasetKind.Courses
        );
        return expect(futureResult).to.be.rejectedWith(InsightError);
    });

    it("test addDataset, given null for id raise InsightError", function () {
        const invalidId: string = null;
        const validId: string = "courses";
        const futureResult: Promise<string[]> = insightFacade.addDataset(
            invalidId,
            datasets[validId],
            InsightDatasetKind.Courses
        );
        return expect(futureResult).to.be.rejectedWith(InsightError);
    });

    it("test addDataset, given valid string of numbers for id return promise", function () {
        const id: string = "123";
        const expected: string[] = [id];
        const futureResult: Promise<string[]> = insightFacade.addDataset(
            id,
            datasets[id],
            InsightDatasetKind.Courses,
        );
        return expect(futureResult).to.eventually.deep.equal(expected);
    });

    it("test addDataset, given id with underscore raise InsightError", function () {
        const invalidId: string = "courses_";
        const validId: string = "courses";
        const futureResult: Promise<string[]> = insightFacade.addDataset(
            invalidId,
            datasets[validId],
            InsightDatasetKind.Courses
        );
        return expect(futureResult).to.be.rejectedWith(InsightError);
    });

    it("test addDataset, given id with only whitespaces raise insightError", function () {
        const invalidId: string = "   ";
        const validId: string = "courses";
        const futureResult: Promise<string[]> = insightFacade.addDataset(
            invalidId,
            datasets[validId],
            InsightDatasetKind.Courses
        );
        return expect(futureResult).to.be.rejectedWith(InsightError);
    });

    // Test addDataset, value and type check content
    it("test addDataset, given empty string for content raise InsightError", function () {
        const id: string = "courses";
        const futureResult: Promise<string[]> = insightFacade.addDataset(
            id,
            "",
            InsightDatasetKind.Courses
        );
        return expect(futureResult).to.be.rejectedWith(InsightError);
    });

    it("test addDataset, given random string for content raise InsightError", function () {
        const id: string = "courses";
        const futureResult: Promise<string[]> = insightFacade.addDataset(
            id,
            "random string",
            InsightDatasetKind.Courses
        );
        return expect(futureResult).to.be.rejectedWith(InsightError);
    });

    it("test addDataset, given string 123 for content raise InsightError", function () {
        const id: string = "courses";
        const futureResult: Promise<string[]> = insightFacade.addDataset(
            id,
            "123",
            InsightDatasetKind.Courses
        );
        return expect(futureResult).to.be.rejectedWith(InsightError);
    });

    it("test addDataset, given undefined for content raise InsightError", function () {
        const id: string = "courses";
        const futureResult: Promise<string[]> = insightFacade.addDataset(
            id,
            undefined,
            InsightDatasetKind.Courses
        );
        return expect(futureResult).to.be.rejectedWith(InsightError);
    });

    it("test addDataset, given null for content raise InsightError", function () {
        const id: string = "courses";
        const futureResult: Promise<string[]> = insightFacade.addDataset(
            id,
            null,
            InsightDatasetKind.Courses
        );
        return expect(futureResult).to.be.rejectedWith(InsightError);
    });

    // TODO: Is this test correct/necessary?
    it("test addDataset, given content not in the form of base64 serialized zip file raise error",
        function () {
            const id: string = "courses";
            const content: string = fs.readFileSync(datasetsToLoad[id]).toString();
            const futureResult: Promise<string[]> = insightFacade.addDataset(
                id,
                content,
                InsightDatasetKind.Courses
            );
            return expect(futureResult).to.be.rejectedWith(InsightError);
        });

    // Test addDataset, Value and type check kind

    it("test addDataset, given undefined for kind raise InsightError", function () {
        const id: string = "courses";
        const futureResult: Promise<string[]> = insightFacade.addDataset(
            id,
            datasets[id],
            undefined,
        );
        return expect(futureResult).to.be.rejectedWith(InsightError);
    });

    it("test addDataset, given null for kind InsightError", function () {
        const id: string = "courses";
        const futureResult: Promise<string[]> = insightFacade.addDataset(
            id,
            datasets[id],
            null,
        );
        return expect(futureResult).to.be.rejectedWith(InsightError);
    });

    it("test add courses dataset but give rooms as kind raise InsightError", function () {
        const id: string = "courses";
        const futureResult: Promise<string[]> = insightFacade.addDataset(
            id,
            datasets[id],
            InsightDatasetKind.Rooms,
        );
        return expect(futureResult).to.be.rejectedWith(InsightError);
    });

    // Test addDataset, sample datasets
    it("test addDataset, given zip file with no courses folder as content raise error", function () {
        const id: string = "zipFileWithNoCourses";
        const futureResult: Promise<string[]> = insightFacade.addDataset(
            id,
            datasets[id],
            InsightDatasetKind.Courses,
        );
        return expect(futureResult).to.be.rejectedWith(InsightError);
    });

    it("test addDataset, given JSON file as content raise error", function () {
        const id: string = "JsonFile";
        const futureResult: Promise<string[]> = insightFacade.addDataset(
            id,
            datasets[id],
            InsightDatasetKind.Courses,
        );
        return expect(futureResult).to.be.rejectedWith(InsightError);
    });

    it("test addDataset, given courses with no valid section as content raise error", function () {
        const id: string = "coursesWithNoValidCourseSection";
        const futureResult: Promise<string[]> = insightFacade.addDataset(
            id,
            datasets[id],
            InsightDatasetKind.Courses,
        );
        return expect(futureResult).to.be.rejectedWith(InsightError);
    });

    it("test addDataset, given courses with valid section as content return expected", function () {
        const id: string = "coursesWithValidCourseSections";
        const expected: string[] = [id];
        const futureResult: Promise<string[]> = insightFacade.addDataset(
            id,
            datasets[id],
            InsightDatasetKind.Courses,
        );
        return expect(futureResult).to.eventually.deep.equal(expected);
    });

    it("test addDataset, given courses no valid JSON as content raise error", function () {
        const id: string = "coursesWithNoValidJson";
        const futureResult: Promise<string[]> = insightFacade.addDataset(
            id,
            datasets[id],
            InsightDatasetKind.Courses,
        );
        return expect(futureResult).to.be.rejectedWith(InsightError);
    });

    it("test addDataset, given content with empty courses folder raise error", function () {
        const id: string = "coursesEmpty";
        const futureResult: Promise<string[]> = insightFacade.addDataset(
            id,
            datasets[id],
            InsightDatasetKind.Courses,
        );
        return expect(futureResult).to.be.rejectedWith(InsightError);
    });

    it("test addDataset, given content with 'hello' and not 'courses' folder raise error",
        function () {
            const id: string = "hello";
            const futureResult: Promise<string[]> = insightFacade.addDataset(
                id,
                datasets[id],
                InsightDatasetKind.Courses,
            );
            return expect(futureResult).to.be.rejectedWith(InsightError);
        });

    it("test addDataset, given content with string of numbers return expected", function () {
        const id: string = "123";
        const expected: string[] = [id];
        const futureResult: Promise<string[]> = insightFacade.addDataset(
            id,
            datasets[id],
            InsightDatasetKind.Courses,
        );
        return expect(futureResult).to.eventually.deep.equal(expected);
    });

    // Test addDataset, functionalities
    it("test addDataset, call method twice with different valid courses ids, return expected promise",
        function () {
            const id1: string = "courses";
            const id2: string = "coursesWithValidCourseSections";
            let expected: string[] = [id1];
            let futureResult: Promise<string[]> = insightFacade.addDataset(
                id1,
                datasets[id1],
                InsightDatasetKind.Courses,
            );
            return futureResult.then(() => {
                expected = [id1, id2];
                let finalResult = insightFacade.addDataset(
                    id2,
                    datasets[id2],
                    InsightDatasetKind.Courses
                );
                return expect(finalResult).to.eventually.deep.equal(expected);
            });
        });

    it("test addDataset, call method twice with different valid rooms ids, return expected promise",
        function () {
            const id1: string = "rooms";
            const id2: string = "roomsWithValidRoom";
            let expected: string[] = [id1];
            let futureResult: Promise<string[]> = insightFacade.addDataset(
                id1,
                datasets[id1],
                InsightDatasetKind.Rooms,
            );
            return futureResult.then(() => {
                expected = [id1, id2];
                let finalResult = insightFacade.addDataset(
                    id2,
                    datasets[id2],
                    InsightDatasetKind.Rooms
                );
                return expect(finalResult).to.eventually.deep.equal(expected);
            });
        });

    it("test addDataset, call method twice with same courses id raise error",
        function () {
            const id: string = "courses";
            const expected: string[] = [id];
            let futureResult: Promise<string[]> = insightFacade.addDataset(
                id,
                datasets[id],
                InsightDatasetKind.Courses,
            );
            return expect(futureResult).to.eventually.deep.equal(expected).then(() => {
                futureResult = insightFacade.addDataset(
                    id,
                    datasets[id],
                    InsightDatasetKind.Courses
                );
                return expect(futureResult).to.be.rejectedWith(InsightError);
            });
        });

    it("test addDataset, call method twice with same rooms id raise error",
        function () {
            const id: string = "roomsWithValidRoom";
            const expected: string[] = [id];
            let futureResult: Promise<string[]> = insightFacade.addDataset(
                id,
                datasets[id],
                InsightDatasetKind.Rooms,
            );
            return expect(futureResult).to.eventually.deep.equal(expected).then(() => {
                futureResult = insightFacade.addDataset(
                    id,
                    datasets[id],
                    InsightDatasetKind.Rooms
                );
                return expect(futureResult).to.be.rejectedWith(InsightError);
            });
        });

    it("test addDataset, given roomsNotInHtml raise InsightError ", function () {
       const id: string = "roomsNotInHtml";
       let futureResult: Promise<string[]> = insightFacade.addDataset(
           id,
           datasets[id],
           InsightDatasetKind.Rooms,
       );
       return expect(futureResult).to.be.rejectedWith(InsightError);
    });

    it("test addDataset, given roomsWithoutAnyRoom raise InsightError", function () {
        const id: string = "roomsWithoutAnyRoom";
        let futureResult: Promise<string[]> = insightFacade.addDataset(
            id,
            datasets[id],
            InsightDatasetKind.Rooms,
        );
        return expect(futureResult).to.be.rejectedWith(InsightError);
    });

    it("test addDataset, given roomsWithRootFolderNamedUbc raise InsightError", function () {
        const id: string = "roomsWithRootFolderNamedUbc";
        let futureResult: Promise<string[]> = insightFacade.addDataset(
            id,
            datasets[id],
            InsightDatasetKind.Rooms,
        );
        return expect(futureResult).to.be.rejectedWith(InsightError);
    });

    it("test addDataset, given roomsWithValidRoom return expected", function () {
        const id: string = "roomsWithValidRoom";
        const expected: string[] = [id];
        const futureResult: Promise<string[]> = insightFacade.addDataset(
            id,
            datasets[id],
            InsightDatasetKind.Rooms,
        );
        return expect(futureResult).to.eventually.deep.equal(expected);
    });

    it("test addDataset, given rooms return expected", function () {
        const id: string = "rooms";
        const expected: string[] = [id];
        const futureResult: Promise<string[]> = insightFacade.addDataset(
            id,
            datasets[id],
            InsightDatasetKind.Rooms,
        );
        return expect(futureResult).to.eventually.deep.equal(expected);
    });


    it("test addDataset, given roomsWithNoIndex raise InsightError", function () {
        const id: string = "roomsWithNoIndex";
        let futureResult: Promise<string[]> = insightFacade.addDataset(
            id,
            datasets[id],
            InsightDatasetKind.Rooms,
        );
        return expect(futureResult).to.be.rejectedWith(InsightError);
    });

    // Test removeDataset, value and type check id
    it("test removeDataset, given empty string for id raise InsightError", function () {
        const id: string = "";
        const futureResult: Promise<string> = insightFacade.removeDataset(id);
        return expect(futureResult).to.be.rejectedWith(InsightError);
    });

    it("test removeDataset, given undefined for id raise InsightError", function () {
        const id: string = undefined;
        const futureResult: Promise<string> = insightFacade.removeDataset(id);
        return expect(futureResult).to.be.rejectedWith(InsightError);
    });

    it("test removeDataset, given null for content kind InsightError", function () {
        const id: string = null;
        const futureResult: Promise<string> = insightFacade.removeDataset(id);
        return expect(futureResult).to.be.rejectedWith(InsightError);
    });

    it("test removeDataset, given valid string of numbers for id return expected", function () {
        const id: string = "123";
        const expected: string = id;
        let promise = insightFacade.addDataset(
            id,
            datasets[id],
            InsightDatasetKind.Courses,
        );
        return promise.then(() => {
            const futureResult: Promise<string> = insightFacade.removeDataset(id);
            return expect(futureResult).to.eventually.deep.equal(expected);
        });
    });

    it("test removeDataset, given roomsWithValidRoom for id return expected", function () {
        const id: string = "roomsWithValidRoom";
        const expected: string = id;
        let promise = insightFacade.addDataset(
            id,
            datasets[id],
            InsightDatasetKind.Rooms,
        );
        return promise.then(() => {
            const futureResult: Promise<string> = insightFacade.removeDataset(id);
            return expect(futureResult).to.eventually.deep.equal(expected);
        });
    });

    it("test removeDataset, given id with underscore raise InsightError", function () {
        const id: string = "courses_";
        const futureResult: Promise<string> = insightFacade.removeDataset(id);
        return expect(futureResult).to.be.rejectedWith(InsightError);
    });

    it("test removeDataset, given id with only whitespace raise InsightError", function () {
        const id: string = "  ";
        const futureResult: Promise<string> = insightFacade.removeDataset(id);
        return expect(futureResult).to.be.rejectedWith(InsightError);
    });

    it("test removeDataset, given id (123) of dataset that hasn't been added raise NotFoundError",
        function () {
            const idToRemove: string = "courses";
            const idToAdd: string = "123";
            let promise = insightFacade.addDataset(idToAdd, datasets[idToAdd], InsightDatasetKind.Courses);
            return promise.then(() => {
                const futureResult: Promise<string> = insightFacade.removeDataset(idToRemove);
                return expect(futureResult).to.be.rejectedWith(NotFoundError);
            });
        });

    it("test removeDataset, given id (roomsWithValidRoom) of dataset that hasn't been added raise NotFoundError",
        function () {
            const idToRemove: string = "roomsWithValidRoom";
            const idToAdd: string = "123";
            let promise = insightFacade.addDataset(idToAdd, datasets[idToAdd], InsightDatasetKind.Courses);
            return promise.then(() => {
                const futureResult: Promise<string> = insightFacade.removeDataset(idToRemove);
                return expect(futureResult).to.be.rejectedWith(NotFoundError);
            });
        });

    it("test removeDataset, given valid id of added courses Dataset call remove twice and raise error",
        function () {
            const id: string = "coursesWithValidCourseSections";
            let promise = insightFacade.addDataset(
                id,
                datasets[id],
                InsightDatasetKind.Courses,
            );
            return promise.then(() => {
                let futureResult: Promise<string> = insightFacade.removeDataset(id);
                return futureResult.then(() => {
                    let finalResult = insightFacade.removeDataset(id);
                    return expect(finalResult).to.be.rejectedWith(NotFoundError);
                });
            });
        });

    it("test removeDataset, given valid id of added rooms Dataset call remove twice and raise error",
        function () {
            const id: string = "roomsWithValidRoom";
            let promise = insightFacade.addDataset(
                id,
                datasets[id],
                InsightDatasetKind.Rooms,
            );
            return promise.then(() => {
                let futureResult: Promise<string> = insightFacade.removeDataset(id);
                return futureResult.then(() => {
                    let finalResult = insightFacade.removeDataset(id);
                    return expect(finalResult).to.be.rejectedWith(NotFoundError);
                });
            });
        });

    // Test listDatasets
    it("test listDataset, with no datasets added return empty array", function () {
        const expected: InsightDataset[] = [];
        const futureResult: Promise<InsightDataset[]> = insightFacade.listDatasets();
        return expect(futureResult).to.eventually.deep.equal(expected);
    });

    it("test listDataset, add courses dataset and return list of array with the dataset", function () {
        const id: string = "courses";
        const expected: InsightDataset[] = [{
            id: id,
            kind: InsightDatasetKind.Courses,
            numRows: 64612
        }];
        let promiseToComplete = insightFacade.addDataset(
            id,
            datasets[id],
            InsightDatasetKind.Courses,
        );
        return promiseToComplete.then(() => {
            const futureResult: Promise<InsightDataset[]> = insightFacade.listDatasets();
            return expect(futureResult).to.eventually.deep.equal(expected);
        });
    });

    it("test listDataset, add rooms dataset and return list of array with the dataset", function () {
        const id: string = "rooms";
        const expected: InsightDataset[] = [{
            id: id,
            kind: InsightDatasetKind.Rooms,
            numRows: 364
        }];
        let promiseToComplete = insightFacade.addDataset(
            id,
            datasets[id],
            InsightDatasetKind.Rooms,
        );
        return promiseToComplete.then(() => {
            const futureResult: Promise<InsightDataset[]> = insightFacade.listDatasets();
            return expect(futureResult).to.eventually.deep.equal(expected);
        });
    });

    it("test listDataset, add and remove dataset, return empty list", function () {
        const id: string = "courses";
        const expected: InsightDataset[] = [];
        let promise1 = insightFacade.addDataset(
            id,
            datasets[id],
            InsightDatasetKind.Courses,
        );
        return promise1.then(() => {
            let promise2 = insightFacade.removeDataset(id);
            return promise2.then(() => {
                const futureResult: Promise<InsightDataset[]> = insightFacade.listDatasets();
                return expect(futureResult).to.eventually.deep.equal(expected);
            });
        });
    });

    it("test listDataset, add two valid courses dataset, return list with both datasets", function () {
        const expected: InsightDataset[] = [
            {id: "123", kind: InsightDatasetKind.Courses, numRows: 64612},
            {id: "courses", kind: InsightDatasetKind.Courses, numRows: 64612}
        ];
        let promisesToComplete: any[] = [];
        let promiseToComplete1 = insightFacade.addDataset(
            "courses",
            datasets["courses"],
            InsightDatasetKind.Courses,
        );
        promisesToComplete.push(promiseToComplete1);
        let promiseToComplete2 = insightFacade.addDataset(
            "123",
            datasets["123"],
            InsightDatasetKind.Courses,
        );
        promisesToComplete.push(promiseToComplete2);
        return Promise.all(promisesToComplete).then(() => {
            const futureResult: Promise<InsightDataset[]> = insightFacade.listDatasets();
            return expect(futureResult).to.eventually.deep.equal(expected);
        });
    });

    it("test listDataset, add two valid rooms dataset, return list with both datasets", function () {
        const expected: InsightDataset[] = [
            {id: "rooms", kind: InsightDatasetKind.Rooms, numRows: 364},
            {id: "roomsWithValidRoom", kind: InsightDatasetKind.Rooms, numRows: 16}
        ];
        let promisesToComplete: any[] = [];
        let promiseToComplete1 = insightFacade.addDataset(
            "rooms",
            datasets["rooms"],
            InsightDatasetKind.Rooms,
        );
        promisesToComplete.push(promiseToComplete1);
        let promiseToComplete2 = insightFacade.addDataset(
            "roomsWithValidRoom",
            datasets["roomsWithValidRoom"],
            InsightDatasetKind.Rooms,
        );
        promisesToComplete.push(promiseToComplete2);
        return Promise.all(promisesToComplete).then(() => {
            const futureResult: Promise<InsightDataset[]> = insightFacade.listDatasets();
            return expect(futureResult).to.eventually.deep.equal(expected);
        });
    });

    it("test listDataset, add both rooms and courses dataset, return list with both datasets", function () {
        const expected: InsightDataset[] = [
            {id: "courses", kind: InsightDatasetKind.Courses, numRows: 64612},
            {id: "rooms", kind: InsightDatasetKind.Rooms, numRows: 364},
        ];
        let promisesToComplete: any[] = [];
        let promiseToComplete1 = insightFacade.addDataset(
            "rooms",
            datasets["rooms"],
            InsightDatasetKind.Rooms,
        );
        promisesToComplete.push(promiseToComplete1);
        let promiseToComplete2 = insightFacade.addDataset(
            "courses",
            datasets["courses"],
            InsightDatasetKind.Courses,
        );
        promisesToComplete.push(promiseToComplete2);
        return Promise.all(promisesToComplete).then(() => {
            const futureResult: Promise<InsightDataset[]> = insightFacade.listDatasets();
            return expect(futureResult).to.eventually.deep.equal(expected);
        });
    });

    it("test listDataset, coursesWithValidCourseSections, return expected", function () {
        const id: string = "coursesWithValidCourseSections";
        const expected: InsightDataset[] = [{
            id: "coursesWithValidCourseSections",
            kind: InsightDatasetKind.Courses,
            numRows: 39
        }];
        let promise = insightFacade.addDataset(
            id,
            datasets[id],
            InsightDatasetKind.Courses,
        );
        return promise.then(() => {
            const futureResult: Promise<InsightDataset[]> = insightFacade.listDatasets();
            return expect(futureResult).to.eventually.deep.equal(expected);
        });
    });

    it("test listDataset, 2 insightfacade instances with courses added, return expected", function () {
        const id: string = "coursesWithValidCourseSections";
        const expected: InsightDataset[] = [{
            id: "coursesWithValidCourseSections",
            kind: InsightDatasetKind.Courses,
            numRows: 39
        }];
        let promise = insightFacade.addDataset(
            id,
            datasets[id],
            InsightDatasetKind.Courses,
        );
        return promise.then(() => {
            let insightFacade2 = new InsightFacade();
            const futureResult: Promise<InsightDataset[]> = insightFacade2.listDatasets();
            return expect(futureResult).to.eventually.deep.equal(expected);
        });
    });

    it("test listDataset, 2 insightfacade instances with rooms added, return expected", function () {
        const id: string = "roomsWithValidRoom";
        const expected: InsightDataset[] = [{
            id: "roomsWithValidRoom",
            kind: InsightDatasetKind.Rooms,
            numRows: 16
        }];
        let promise = insightFacade.addDataset(
            id,
            datasets[id],
            InsightDatasetKind.Rooms,
        );
        return promise.then(() => {
            let insightFacade2 = new InsightFacade();
            const futureResult: Promise<InsightDataset[]> = insightFacade2.listDatasets();
            return expect(futureResult).to.eventually.deep.equal(expected);
        });
    });

    it("Call perform query with a string", function () {
        let futureResult: Promise<any[]> = insightFacade.performQuery("foo bar");
        return expect(futureResult).to.be.rejectedWith(InsightError);
    });

    it("Call perform query with a numbers", function () {
        let futureResult: Promise<any[]> = insightFacade.performQuery(123);
        return expect(futureResult).to.be.rejectedWith(InsightError);
    });

    it("Call perform query with an array", function () {
        let futureResult: Promise<any[]> = insightFacade.performQuery(["foo", 123]);
        return expect(futureResult).to.be.rejectedWith(InsightError);
    });

    it("Call perform query with an empty array", function () {
        let futureResult: Promise<any[]> = insightFacade.performQuery([]);
        return expect(futureResult).to.be.rejectedWith(InsightError);
    });

    it("Call perform query with an null", function () {
        let futureResult: Promise<any[]> = insightFacade.performQuery(null);
        return expect(futureResult).to.be.rejectedWith(InsightError);
    });

    it("Call perform query with an undefined", function () {
        let futureResult: Promise<any[]> = insightFacade.performQuery(undefined);
        return expect(futureResult).to.be.rejectedWith(InsightError);
    });

    it("Call perform query with an NaN", function () {
        let futureResult: Promise<any[]> = insightFacade.performQuery(NaN);
        return expect(futureResult).to.be.rejectedWith(InsightError);
    });

    it("Call perform query with an true", function () {
        let futureResult: Promise<any[]> = insightFacade.performQuery(true);
        return expect(futureResult).to.be.rejectedWith(InsightError);
    });

    it("Call perform query with an WHERE: NULL", function () {
        let futureResult: Promise<any[]> = insightFacade.performQuery({WHERE: null });
        return expect(futureResult).to.be.rejectedWith(InsightError);
    });

    it("Call perform query with an WHERE: undefined", function () {
        let futureResult: Promise<any[]> = insightFacade.performQuery({WHERE: undefined });
        return expect(futureResult).to.be.rejectedWith(InsightError);
    });

    it("Call perform query with an empty object", function () {
        let futureResult: Promise<any[]> = insightFacade.performQuery({});
        return expect(futureResult).to.be.rejectedWith(InsightError);
    });

    it("Call perform query with an null inside object", function () {
        let futureResult: Promise<any[]> = insightFacade.performQuery({null: null});
        return expect(futureResult).to.be.rejectedWith(InsightError);
    });

    it("Call perform query with an null inside object", function () {
        let futureResult: Promise<any[]> = insightFacade.performQuery({undefined: undefined});
        return expect(futureResult).to.be.rejectedWith(InsightError);
    });
});

/*
 * This test suite dynamically generates tests from the JSON files in test/queries.
 * You should not need to modify it; instead, add additional files to the queries directory.
 * You can still make tests the normal way, this is just a convenient tool for a majority of queries.
 */
describe("InsightFacade PerformQuery", () => {
    const datasetsToQuery: {
        [id: string]: { path: string; kind: InsightDatasetKind };
    } = {
        courses: {
            path: "./test/data/courses.zip",
            kind: InsightDatasetKind.Courses,
        },
        rooms: {
            path: "./test/data/rooms.zip",
            kind: InsightDatasetKind.Rooms,
        }
    };
    let insightFacade: InsightFacade;
    let testQueries: ITestQuery[] = [];

    // Load all the test queries, and call addDataset on the insightFacade instance for all the datasets
    before(function () {
        Log.test(`Before: ${this.test.parent.title}`);

        // Load the query JSON files under test/queries.
        // Fail if there is a problem reading ANY query.
        try {
            testQueries = TestUtil.readTestQueries();
        } catch (err) {
            expect.fail(
                "",
                "",
                `Failed to read one or more test queries. ${err}`,
            );
        }

        // Load the datasets specified in datasetsToQuery and add them to InsightFacade.
        // Will fail* if there is a problem reading ANY dataset.
        const loadDatasetPromises: Array<Promise<string[]>> = [];
        insightFacade = new InsightFacade();
        for (const id of Object.keys(datasetsToQuery)) {
            const ds = datasetsToQuery[id];
            const data = fs.readFileSync(ds.path).toString("base64");
            loadDatasetPromises.push(
                insightFacade.addDataset(id, data, ds.kind),
            );
        }
        return Promise.all(loadDatasetPromises);
    });

    beforeEach(function () {
        Log.test(`BeforeTest: ${this.currentTest.title}`);
    });

    after(function () {
        Log.test(`After: ${this.test.parent.title}`);
    });

    afterEach(function () {
        Log.test(`AfterTest: ${this.currentTest.title}`);
    });

    // Dynamically create and run a test for each query in testQueries
    // Creates an extra "test" called "Should run test queries" as a byproduct. Don't worry about it
    it("Should run test queries", function () {
        describe("Dynamic InsightFacade PerformQuery tests", function () {
            for (const test of testQueries) {
                it(`[${test.filename}] ${test.title}`, function () {
                    if (test.filename === "test/queries/c2simple(no_order).json") {
                    const futureResult: Promise<
                        any[]
                        > = insightFacade.performQuery(test.query);
                    return TestUtil.verifyQueryResult(futureResult, test);
                    }
                });
            }
        });
    });
});
