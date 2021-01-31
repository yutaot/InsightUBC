import {InsightDatasetKind, InsightError} from "./IInsightFacade";
import * as JSZip from "jszip";
import * as fs from "fs";

export default abstract class StoreDataset {

    abstract validateRootFolder(unzippedFile: any): boolean;

    abstract validateAddedDatasetResult(datasetsObject: any, id: string): boolean;

    abstract determineDatasetKindObject(kind: InsightDatasetKind): any;

    protected storeDatasetToDisk (
        id: string, promisesToExecute: any[], datasetsObject: any, otherDatasetsObject: any, datasetsPathKind: string,
        resolve: any, reject: any
    ): any {
        Promise.all(promisesToExecute).then(() => {
            if (!this.validateAddedDatasetResult(datasetsObject, id)) {
                return reject(new InsightError());
            }
            return fs.writeFile(
                datasetsPathKind,
                JSON.stringify(datasetsObject, null, 2),
                "utf-8",
                () => {
                    return resolve(Object.keys(datasetsObject).concat(Object.keys(otherDatasetsObject)));
                }
            );
        });
    }

    abstract storeDatasetToMemoryAndDiskHelper(
        id: string, unzippedFile: any, datasetsObject: any, otherDatasetsObject: any, datasetsPath: string,
        resolve: any, reject: any): any;

    /**
     * This method is addDataset's helper for doing bulk of the work for storing all course sections in a
     * zip file to the local variable attribute of this class "this.datasets", and stores this local
     * variable to disk as a json file in the path "./data/datasets.json". The promise returned by this method
     * is exactly the same as addDataset().
     *
     *
     * @param id  The id of the dataset being added. Follows the format /^[^_]+$/
     * @param content  The base64 content of the dataset. This content should be in the form of a serialized zip file.
     * @param kind  The kind of the dataset
     *
     * @return Promise <string[]>
     */
    public storeDatasetToMemoryAndDisk(
        id: string, content: string, kind: InsightDatasetKind, datasetsObject: any, otherDatasetsObject: any,
        datasetsPathKind: string
    ): Promise<string[]> {
        let zip: JSZip = new JSZip();
        return new Promise( (resolve, reject) => {
            zip.loadAsync(content, {base64: true}).then((unzippedFile) => {
                if (!(this.validateRootFolder(unzippedFile))) {
                    return reject(new InsightError());
                }
                datasetsObject[id] = this.determineDatasetKindObject(kind);
                return this.storeDatasetToMemoryAndDiskHelper(id, unzippedFile, datasetsObject, otherDatasetsObject,
                    datasetsPathKind, resolve, reject);
            }).catch(() => {
                return reject(new InsightError());
            });
        });
    }
}
