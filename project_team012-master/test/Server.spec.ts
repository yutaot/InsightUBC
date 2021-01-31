import Server from "../src/rest/Server";

import InsightFacade from "../src/controller/InsightFacade";
import {expect} from "chai";
import Log from "../src/Util";
import * as fs from "fs-extra";
import {InsightDatasetKind} from "../src/controller/IInsightFacade";
import chai = require("chai");
import chaiHttp = require("chai-http");
import Response = ChaiHttp.Response;
import TestUtil from "./TestUtil";
import {ITestQuery} from "./InsightFacade.spec";

let SERVER_URL: string = "http://localhost:4321";

describe("Facade D3", function () {

    let facade: InsightFacade = null;
    let server: Server = null;
    const cacheDir = __dirname + "/../data";
    const datasetsToLoad: { [id: string]: string } = {
        courses: "./test/data/courses.zip",
        zipFileWithNoCourses: "./test/data/zipFileWIthNoCourses.zip",             // Invalid
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
    let datasets: { [id: string]: Buffer } = {};
    let testQueries: ITestQuery[] = [];
    chai.use(chaiHttp);

    before(function () {
        Log.test(`Before all`);
        for (const id of Object.keys(datasetsToLoad)) {
            datasets[id] = fs.readFileSync(datasetsToLoad[id]);
        }
        facade = new InsightFacade();
        server = new Server(4321);
        try {
            testQueries = TestUtil.readTestQueries();
        } catch (err) {
            expect.fail(
                "",
                "",
                `Failed to read one or more test queries. ${err}`,
            );
        }
        // TODO: start server here once and handle errors properly
        try {
            return server.start();
        } catch (e) {
            Log.error(e);
        }
    });

    after(function () {
        // TODO: stop server here once!
        Log.test(`After: ${this.test.parent.title}`);
        return server.stop();
    });

    beforeEach(function () {
        // might want to add some process logging here to keep track of what"s going on
        Log.test(`BeforeTest: ${this.currentTest.title}`);
    });

    afterEach(function () {
        // might want to add some process logging here to keep track of what"s going on
        Log.test(`AfterTest: ${this.currentTest.title}`);
    });

    // Sample on how to format PUT requests
    it("PUT test for coursesWithValidCourseSections", function () {
        let id: string = "coursesWithValidCourseSections";
        try {
            return chai.request(SERVER_URL)
                .put("/dataset/coursesWithValidCourseSections/courses")
                .send(datasets[id])
                .set("Content-Type", "application/x-zip-compressed")
                .then(function (res: Response) {
                    // some logging here please!
                    Log.info("PUT test for courses dataset - responding " + res.status);
                    expect(res.status).to.be.equal(200);
                })
                .catch(function (err) {
                    // some logging here please!
                    Log.info("PUT test for courses dataset - responding " + err);
                    expect.fail();
                });
        } catch (err) {
            // and some more logging here!
            Log.info("PUT test for courses dataset - responding " + err);
        }
    });

    it("PUT test with 2 instances of facade adding same dataset, res=400", function () {
        let id: string = "123";
        try {
            return chai.request(SERVER_URL)
                .put("/dataset/123/courses")
                .send(datasets[id])
                .set("Content-Type", "application/x-zip-compressed")
                .then(function (res1: Response) {
                    // some logging here please!
                    Log.info("PUT test for courses dataset - responding " + res1.status);
                    server.stop();
                    server.start();
                    return chai.request(SERVER_URL)
                        .put("/dataset/123/courses")
                        .send(datasets[id])
                        .set("Content-Type", "application/x-zip-compressed")
                        .then(function (res2: Response) {
                            // some logging here please!
                            Log.info("PUT test for courses dataset - responding " + res2.status);
                            expect.fail();
                        })
                        .catch(function (err) {
                            // some logging here please!
                            Log.info("PUT test for courses dataset - responding " + err);
                            expect(err.status).to.be.equal(400);
                        });
                })
                .catch(function (err) {
                    // some logging here please!
                    Log.info("PUT test for courses dataset - responding " + err);
                    expect.fail();
                });
        } catch (err) {
            // and some more logging here!
            Log.info("PUT test for courses dataset - responding " + err);
        }
    });

    it("PUT test for coursesWithNoValidCourseSection", function () {
        let id: string = "coursesWithNoValidCourseSection";
        try {
            return chai.request(SERVER_URL)
                .put("/dataset/coursesWithNoValidCourseSection/courses")
                .send(datasets[id])
                .set("Content-Type", "application/x-zip-compressed")
                .then(function (res: Response) {
                    // some logging here please!
                    Log.info("PUT test for courses dataset - responding " + res.status);
                    expect.fail();
                })
                .catch(function (err) {
                    // some logging here please!
                    Log.info("PUT test for courses dataset - responding " + err);
                    expect(err.response.status).to.be.equal(400);
                });
        } catch (err) {
            // and some more logging here!
            Log.info("PUT test for courses dataset - responding " + err);
        }
    });

    it("PUT test for roomsWithValidRoom", function () {
        let id: string = "roomsWithValidRoom";
        try {
            return chai.request(SERVER_URL)
                .put("/dataset/roomsWithValidRoom/rooms")
                .send(datasets[id])
                .set("Content-Type", "application/x-zip-compressed")
                .then(function (res: Response) {
                    // some logging here please!
                    Log.info("PUT test for rooms dataset - responding " + res.status);
                    expect(res.status).to.be.equal(200);
                })
                .catch(function (err) {
                    // some logging here please!
                    Log.info("PUT test for rooms dataset - responding " + err);
                    expect.fail();
                });
        } catch (err) {
            // and some more logging here!
            Log.info("PUT test for rooms dataset - responding " + err);
        }
    });

    it("PUT test for adding roomsWithValidRoom again, res = 400", function () {
        let id: string = "roomsWithValidRoom";
        try {
            return chai.request(SERVER_URL)
                .put("/dataset/roomsWithValidRoom/rooms")
                .send(datasets[id])
                .set("Content-Type", "application/x-zip-compressed")
                .then(function (res: Response) {
                    // some logging here please!
                    Log.info("PUT test for rooms dataset - responding " + res.status);
                    expect.fail();
                })
                .catch(function (err) {
                    // some logging here please!
                    Log.info("PUT test for rooms dataset - responding " + err);
                    expect(err.response.status).to.be.equal(400);
                });
        } catch (err) {
            // and some more logging here!
            Log.info("PUT test for rooms dataset - responding " + err);
        }
    });

    it("PUT test for roomsWithoutAnyRoom", function () {
        let id: string = "roomsWithoutAnyRoom";
        try {
            return chai.request(SERVER_URL)
                .put("/dataset/roomsWithoutAnyRoom/courses")
                .send(datasets[id])
                .set("Content-Type", "application/x-zip-compressed")
                .then(function (res: Response) {
                    // some logging here please!
                    Log.info("PUT test for courses dataset - responding " + res.status);
                    expect.fail();
                })
                .catch(function (err) {
                    // some logging here please!
                    Log.info("PUT test for courses dataset - responding " + err);
                    expect(err.response.status).to.be.equal(400);
                });
        } catch (err) {
            // and some more logging here!
            Log.info("PUT test for courses dataset - responding " + err);
        }
    });

    it("DELETE test for removing dataset before adding, res = 404", function () {
        try {
            return chai.request(SERVER_URL)
                .del("/dataset/roomsWithoutAnyRoom")
                .then(function (res: Response) {
                    Log.info("DELETE test for removing dataset before adding - responding " + res.status);
                    expect.fail();
                })
                .catch(function (err) {
                    Log.info("DELETE test for removing dataset before adding - responding " + err);
                    expect(err.response.status).to.be.equal(404);
                });
        } catch (err) {
            // and some more logging here!
            Log.info("DELETE test for removing dataset before adding - responding " + err);
        }
    });

    it("DELETE test with invalid ID, res = 400", function () {
        try {
            return chai.request(SERVER_URL)
                .del("/dataset/rooms_123")
                .then(function (res: Response) {
                    Log.info("DELETE test with invalid ID - responding " + res.status);
                    expect.fail();
                })
                .catch(function (err) {
                    Log.info("DELETE test with invalid ID - responding " + err);
                    expect(err.response.status).to.be.equal(400);
                });
        } catch (err) {
            // and some more logging here!
            Log.info("DELETE test with invalid ID - responding " + err);
        }
    });

    it("DELETE coursesWithValidCourseSections after adding, res = 200", function () {
        try {
            return chai.request(SERVER_URL)
                .del("/dataset/coursesWithValidCourseSections")
                .then(function (delRes: Response) {
                    Log.info("DELETE test with invalid ID - responding " + delRes.status);
                    expect(delRes.status).to.be.equal(200);
                })
                .catch(function (err) {
                    Log.info("DELETE test with invalid ID - responding " + err);
                    expect.fail();
                });
        } catch (err) {
            // and some more logging here!
            Log.info("DELETE test with invalid ID - responding " + err);
        }
    });

    it("DELETE 123 after adding, res = 200", function () {
        try {
            return chai.request(SERVER_URL)
                .del("/dataset/123")
                .then(function (delRes: Response) {
                    Log.info("DELETE test with invalid ID - responding " + delRes.status);
                    expect(delRes.status).to.be.equal(200);
                })
                .catch(function (err) {
                    Log.info("DELETE test with invalid ID - responding " + err);
                    expect.fail();
                });
        } catch (err) {
            // and some more logging here!
            Log.info("DELETE test with invalid ID - responding " + err);
        }
    });

    it("DELETE roomsWithValidRoom after adding, res = 200", function () {
        try {
            return chai.request(SERVER_URL)
                .del("/dataset/roomsWithValidRoom")
                .then(function (delRes: Response) {
                    Log.info("DELETE test with roomsWithValidRoom - responding " + delRes.status);
                    expect(delRes.status).to.be.equal(200);
                })
                .catch(function (err) {
                    Log.info("DELETE test with roomsWithValidRoom - responding " + err);
                    expect.fail();
                });
        } catch (err) {
            // and some more logging here!
            Log.info("DELETE test with invalid ID - responding " + err);
        }
    });

    it("GET test for listDataset before adding, res = 200", function () {
        try {
            return chai.request(SERVER_URL)
                .get("/datasets")
                .then(function (res: Response) {
                    Log.info("GET test for listDataset before adding - responding " + res.status);
                    expect(res.status).to.be.equal(200);
                })
                .catch(function (err) {
                    Log.info("GET test for listDataset before adding - responding " + err);
                    expect.fail();
                });
        } catch (err) {
            // and some more logging here!
            Log.info("GET test for listDataset before adding - responding " + err);
        }
    });

    it("PUT test for courses + rooms with 2 instances of server start/stop", function () {
        try {
            return chai.request(SERVER_URL)
                .put("/dataset/courses/courses")
                .send(datasets["courses"])
                .set("Content-Type", "application/x-zip-compressed")
                .then(function (res1: Response) {
                    // some logging here please!
                    Log.info("PUT test for courses dataset - responding " + res1.status);
                    server.stop();
                    server.start();
                    return chai.request(SERVER_URL)
                        .put("/dataset/rooms/rooms")
                        .send(datasets["rooms"])
                        .set("Content-Type", "application/x-zip-compressed")
                        .then(function (res2: Response) {
                            // some logging here please!
                            Log.info("PUT test for rooms dataset - responding " + res2.status);
                            expect(res2.body.result).to.deep.equal(["rooms", "courses"]);
                        })
                        .catch(function (err) {
                            // some logging here please!
                            Log.info("PUT test for rooms dataset - responding " + err);
                            expect.fail(err);
                        });
                })
                .catch(function (err) {
                    // some logging here please!
                    Log.info("PUT test for courses dataset - responding " + err);
                    expect.fail(err);
                });
        } catch (err) {
            // and some more logging here!
            Log.info("PUT test for courses dataset - responding " + err);
        }
    });

    // it("PUT test for rooms", function () {
    //     let id: string = "rooms";
    //     try {
    //         return chai.request(SERVER_URL)
    //             .put("/dataset/rooms/rooms")
    //             .send(datasets[id])
    //             .set("Content-Type", "application/x-zip-compressed")
    //             .then(function (res: Response) {
    //                 // some logging here please!
    //                 Log.info("PUT test for rooms dataset - responding " + res.status);
    //                 expect(res.status).to.be.equal(200);
    //             })
    //             .catch(function (err) {
    //                 // some logging here please!
    //                 Log.info("PUT test for rooms dataset - responding " + err);
    //                 expect.fail();
    //             });
    //     } catch (err) {
    //         // and some more logging here!
    //         Log.info("PUT test for rooms dataset - responding " + err);
    //     }
    // });

    it("POST test for query simple.json", function () {
        let content: Buffer = fs.readFileSync(__dirname + "/queries/simple.json");
        let query = JSON.parse(content.toString()).query;
        try {
            return chai.request(SERVER_URL)
                .post("/query")
                .send(query)
                .then(function (res: Response) {
                    expect(res.status).to.be.equal(200);
                })
                .catch(function (err) {
                    Log.info(err);
                    expect.fail();
                });
        } catch (err) {
            expect.fail();
        }
    });

    it("POST test for query invalid.json", function () {
        let content: Buffer = fs.readFileSync(__dirname + "/queries/invalid.json");
        let query = JSON.parse(content.toString()).query;
        try {
            return chai.request(SERVER_URL)
                .post("/query")
                .send(query)
                .then(function (res: Response) {
                    Log.info(res);
                    expect.fail();
                })
                .catch(function (err) {
                    expect(err.response.status).to.be.equal(400);
                });
        } catch (err) {
            expect.fail();
        }
    });

    it("POST test for query c2complex.json", function () {
        let content: Buffer = fs.readFileSync(__dirname + "/queries/c2complex.json");
        let query = JSON.parse(content.toString()).query;
        try {
            return chai.request(SERVER_URL)
                .post("/query")
                .send(query)
                .set("Content-Type", "application/json")
                .then(function (res: Response) {
                    expect(res.status).to.be.equal(200);
                })
                .catch(function (err) {
                    Log.info(err);
                    expect.fail();
                });
        } catch (err) {
            expect.fail();
        }
    });

    it("POST test for query invalidReferenceDatasetNotAddedYet.json", function () {
        let content: Buffer = fs.readFileSync(__dirname + "/queries/invalidReferenceDatasetNotAddedYet.json");
        let query = JSON.parse(content.toString()).query;
        try {
            return chai.request(SERVER_URL)
                .post("/query")
                .send(query)
                .set("Content-Type", "application/json")
                .then(function (res: Response) {
                    Log.info(res);
                    expect.fail();
                })
                .catch(function (err) {
                    expect(err.response.status).to.be.equal(400);
                });
        } catch (err) {
            expect.fail();
        }
    });

    it("POST test for query invalidTooLarge.json", function () {
        let content: Buffer = fs.readFileSync(__dirname + "/queries/invalidTooLarge.json");
        let query = JSON.parse(content.toString()).query;
        try {
            return chai.request(SERVER_URL)
                .post("/query")
                .send(query)
                .set("Content-Type", "application/json")
                .then(function (res: Response) {
                    Log.info(res);
                    expect.fail();
                })
                .catch(function (err) {
                    expect(err.response.status).to.be.equal(400);
                });
        } catch (err) {
            expect.fail();
        }
    });

    it("POST test for query c2invalidTransformExtraKeys.json", function () {
        let content: Buffer = fs.readFileSync(__dirname + "/queries/c2invalidTransformExtraKeys.json");
        let query = JSON.parse(content.toString()).query;
        try {
            return chai.request(SERVER_URL)
                .post("/query")
                .send(query)
                .set("Content-Type", "application/json")
                .then(function (res: Response) {
                    Log.info(res);
                    expect.fail();
                })
                .catch(function (err) {
                    expect(err.response.status).to.be.equal(400);
                });
        } catch (err) {
            expect.fail();
        }
    });
});

// // TODO: TA -- do queries here
// describe("Facade D3 Queries", function () {
//
//     let facade: InsightFacade = null;
//     let server: Server = null;
//     const cacheDir = __dirname + "/../data";
//
//     const datasetsToQuery: {
//         [id: string]: { path: string; kind: InsightDatasetKind };
//     } = {
//         courses: {
//             path: "./test/data/courses.zip",
//             kind: InsightDatasetKind.Courses,
//         },
//         rooms: {
//             path: "./test/data/rooms.zip",
//             kind: InsightDatasetKind.Rooms,
//         }
//     };
//     let datasets: { [id: string]: string } = {};
//
//     chai.use(chaiHttp);
//
//     before(function () {
//         facade = new InsightFacade();
//         server = new Server(4321);
//         // will remove anything from the cached folder
//         if (!fs.existsSync(cacheDir)) {
//             fs.mkdirSync(cacheDir);
//         }
//         // REMEMBER THAT THESE ARE PROMISES, CHAIN THEN PROPERLY
//         // TODO step 1 and 2, facade add courses and rooms datasets
//         const loadDatasetPromises: Array<Promise<string[]>> = [];
//         for (const id of Object.keys(datasetsToQuery)) {
//             const ds = datasetsToQuery[id];
//             const data = fs.readFileSync(ds.path).toString("base64");
//             loadDatasetPromises.push(
//                 facade.addDataset(id, data, ds.kind),
//             );
//         }
//         // TODO step 3
//         //  start server here once and handle errors properly
//         return Promise.all(loadDatasetPromises).then(() => {
//             try {
//                 return server.start();
//             } catch (e) {
//                 Log.error(e);
//             }
//         });
//     });
//
//     after(function () {
//         // will put the cached folder back for any follow-up tests
//         try {
//             fs.removeSync(cacheDir);
//             fs.mkdirSync(cacheDir);
//         } catch (err) {
//             Log.error(err);
//         }
//         return server.stop();
//     });
//
//     beforeEach(function () {
//         // might want to add some process logging here to keep track of what"s going on
//     });
//
//     afterEach(function () {
//         // might want to add some process logging here to keep track of what"s going on
//     });
//
//     it("POST test for query simple.json", function () {
//         let content: Buffer = fs.readFileSync(__dirname + "/queries/simple.json");
//         let query = JSON.parse(content.toString()).query;
//         try {
//             return chai.request(SERVER_URL)
//                 .post("/query")
//                 .send(query)
//                 .then(function (res: Response) {
//                     expect(res.status).to.be.equal(200);
//                 })
//                 .catch(function (err) {
//                     expect.fail();
//                 });
//         } catch (err) {
//             expect.fail();
//         }
//     });
//
//     it("POST test for query invalid.json", function () {
//         let content: Buffer = fs.readFileSync(__dirname + "/queries/invalid.json");
//         let query = JSON.parse(content.toString()).query;
//         try {
//             return chai.request(SERVER_URL)
//                 .post("/query")
//                 .send(query)
//                 .then(function (res: Response) {
//                     expect.fail();
//                 })
//                 .catch(function (err) {
//                     expect(err.response.status).to.be.equal(400);
//                 });
//         } catch (err) {
//             expect.fail();
//         }
//     });
//
// });
