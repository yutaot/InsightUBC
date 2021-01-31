import StoreDataset from "./StoreDataset";
import Log from "../Util";
import {CourseSection, InsightDatasetKind, InsightError} from "./IInsightFacade";
import * as fs from "fs";

export default class StoreCoursesDataset extends StoreDataset {
    // TODO: Should I change all methods here to protected?

    public validateRootFolder(unzippedFile: any): boolean {
        return "courses/" in unzippedFile.files;
    }

    public determineDatasetKindObject(kind: InsightDatasetKind): any {
        return {sections: [], kind: kind};
    }

    /**
     * Converts courseFileData into list of JSON objects where each object represents
     * a course section, then push each section as a CourseSection object to the
     * "this.datasets[id].sections" variable of this class to store into memory.
     *
     * @param id  The id of the dataset being added. Follows the format /^[^_]+$/
     * @param courseFileData The string representing the contents of a course file
     *
     * @return void
     */
    public storeFileDataToMemory(id: string, courseFileData: string, coursesDatasets: any): void {
        let jsonCourseSectionsList = JSON.parse(courseFileData).result;
        jsonCourseSectionsList.forEach((jsonCourseSection: any) => {
            if (
                (jsonCourseSection == null) ||
                (jsonCourseSection.Subject == null) ||
                (jsonCourseSection.Course == null) ||
                (jsonCourseSection.Avg == null) ||
                (jsonCourseSection.Professor == null) ||
                (jsonCourseSection.Title == null) ||
                (jsonCourseSection.Pass == null) ||
                (jsonCourseSection.Fail == null) ||
                (jsonCourseSection.Audit == null) ||
                (jsonCourseSection.id == null) ||
                (jsonCourseSection.Year == null)
            ) {
                return;
            }
            let courseSection: CourseSection = {
                dept: String(jsonCourseSection.Subject),
                id: String(jsonCourseSection.Course),
                avg: Number(jsonCourseSection.Avg),
                instructor: String(jsonCourseSection.Professor),
                title: String(jsonCourseSection.Title),
                pass: Number(jsonCourseSection.Pass),
                fail: Number(jsonCourseSection.Fail),
                audit: Number(jsonCourseSection.Audit),
                uuid: String(jsonCourseSection.id),
                year: ((jsonCourseSection.Section === "overall") ? 1900 : Number(jsonCourseSection.Year)),
            };
            coursesDatasets[id].sections.push(courseSection);
        });
    }

    public storeDatasetFilesToMemory(
        id: string, coursesFilesObject: object, coursesDatasets: any, promisesToExecute: any[]
    ): any[] {
        Object.values(coursesFilesObject).forEach((file) => {
            let promiseToExecute = file.async("string").then((fileData: string) => {
                try {
                    this.storeFileDataToMemory(id, fileData, coursesDatasets);
                } catch (e) {
                    Log.trace("Failure to parse data for a file, skipping over...");
                }
            });
            promisesToExecute.push(promiseToExecute);
        });
        return promisesToExecute;
    }

    public storeDatasetToMemory(id: string, unzippedFile: any, coursesDatasets: object): any[] {
        let courseFiles: object = unzippedFile.folder("courses").files;
        let promisesToExecute: any[] = [];
        promisesToExecute = this.storeDatasetFilesToMemory(id, courseFiles, coursesDatasets, promisesToExecute);
        return promisesToExecute;
    }

    public validateAddedDatasetResult(coursesDatasets: any, id: string): boolean {
        if (!coursesDatasets[id].sections) {
            delete coursesDatasets[id];
            return false;
        }
        if (coursesDatasets[id].sections.length === 0) {
            delete coursesDatasets[id];
            return false;
        }
        return true;
    }

    // public storeDatasetToDisk (
    //     id: string, datasetsObject: any, datasetsPathKind: string, resolve: any, reject: any
    // ): any {
    //     if (!this.validateAddedDatasetResult(datasetsObject, id)) {
    //         reject(new InsightError());
    //     }
    //     return fs.writeFile(
    //         datasetsPathKind,
    //         JSON.stringify(datasetsObject, null, 2),
    //         "utf-8",
    //         () => {
    //             return resolve(Object.keys(datasetsObject));
    //         }
    //     );
    // }

    public storeDatasetToMemoryAndDiskHelper(id: string, unzippedFile: any, coursesDatasets: any, roomsDatasets: any,
                                             coursesDatasetsPath: string, resolve: any, reject: any) {
        try {
            let promisesToExecute = this.storeDatasetToMemory(id, unzippedFile, coursesDatasets);
            return this.storeDatasetToDisk(
                id, promisesToExecute, coursesDatasets, roomsDatasets, coursesDatasetsPath, resolve, reject
            );
        } catch (e) {
            reject(new InsightError());
        }
    }
}
