import { InsightDatasetKind, CourseSection, InsightError } from "./IInsightFacade";

/**
 * Validates the id of a dataset, returns false if the id is invalid, and true if it's valid
 * @param id  The id of the dataset being validated
 * @return boolean
 */
export function validateId(id: string): boolean {
    if (id === "") {
        return false;
    }
    if (id === null) {
        return false;
    }
    if (id === undefined) {
        return false;
    }
    if (id.includes("_")) {
        return false;
    }
    // check if id is all whitespaces
    if (id.match(/^\s*$/) !== null) {
        return false;
    }
    return true;
}

/**
 * Validates the content of a dataset, returns false if the content is invalid, and true if it's valid
 * @param content  The content of the dataset being validated
 * @return boolean
 */
export function validateContent(content: string): boolean {
    if (content === null) {
        return false;
    }
    return true;
}

/**
 * Validates the kind of a dataset, returns false if the kind is invalid, and true if it's valid
 * @param content  The kind of the dataset being validated
 * @return boolean
 */
export function validateKind(kind: InsightDatasetKind): boolean {
    if (kind === undefined) {
        return false;
    }
    if (kind === null) {
        return false;
    }
    return true;
}

export function isIterable(obj: any) {
    if (obj == null) {
        return false;
    }
    return typeof obj[Symbol.iterator] === "function";
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
// export function storeCourseSectionsToMemory(id: string, courseFileData: string, coursesDatasets: any): void {
//     let jsonCourseSectionsList = JSON.parse(courseFileData).result;
//     jsonCourseSectionsList.forEach((jsonCourseSection: any) => {
//         if (
//             (jsonCourseSection == null) ||
//             (jsonCourseSection.Subject == null) ||
//             (jsonCourseSection.Course == null) ||
//             (jsonCourseSection.Avg == null) ||
//             (jsonCourseSection.Professor == null) ||
//             (jsonCourseSection.Title == null) ||
//             (jsonCourseSection.Pass == null) ||
//             (jsonCourseSection.Fail == null) ||
//             (jsonCourseSection.Audit == null) ||
//             (jsonCourseSection.id == null) ||
//             (jsonCourseSection.Year == null)
//         ) {
//             return;
//         }
//         let courseSection: CourseSection = {
//             dept: String(jsonCourseSection.Subject),
//             id: String(jsonCourseSection.Course),
//             avg: Number(jsonCourseSection.Avg),
//             instructor: String(jsonCourseSection.Professor),
//             title: String(jsonCourseSection.Title),
//             pass: Number(jsonCourseSection.Pass),
//             fail: Number(jsonCourseSection.Fail),
//             audit: Number(jsonCourseSection.Audit),
//             uuid: String(jsonCourseSection.id),
//             year: ((jsonCourseSection.Section === "overall") ? 1900 : Number(jsonCourseSection.Year)),
//         };
//         coursesDatasets[id].sections.push(courseSection);
//     });
// }

// export function storeCoursesDatasetToMemory(id: string, courseFiles: object, coursesDatasets: object): any[] {
//     let promisesToExecute: any[] = [];
//     Object.values(courseFiles).forEach((courseFile) => {
//         let promiseToExecute = courseFile.async("string").then((courseFileData: string) => {
//             try {
//                 storeCourseSectionsToMemory(id, courseFileData, coursesDatasets);
//             } catch (e) {
//                 Log.trace("Failure to parse data for a file, skipping over...");
//             }
//         });
//         promisesToExecute.push(promiseToExecute);
//     });
//     return promisesToExecute;
// }

// export function storeRoomsDatasetToMemoryAndDisk(
//     id: string, content: string, kind: InsightDatasetKind, roomsDatasets: any, roomsDatasetsPath: string
// ): Promise<string[]> {
//     let zip: JSZip = new JSZip();
//     return new Promise( (resolve, reject) => {
//         zip.loadAsync(content, {base64: true}).then((unzippedFile) => {
//             if (!("rooms/" in unzippedFile.files)) {
//                 reject(new InsightError());
//             }
//             let courses = unzippedFile.folder("rooms");
//             roomsDatasets[id] = {sections: [], kind: kind};
//
//             let promisesToExecute = storeRoomsDatasetToMemory(id, courses.files, roomsDatasets);
//             return storeDatasetToDisk(
//                 id, promisesToExecute, roomsDatasets, roomsDatasetsPath, resolve, reject
//             );
//         }).catch(() => {
//             reject(new InsightError());
//         });
//     });
// }
