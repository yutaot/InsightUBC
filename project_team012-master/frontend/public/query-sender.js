/**
 * Receives a query object as parameter and sends it as Ajax request to the POST /query REST endpoint.
 *
 * @param query The query object
 * @returns {Promise} Promise that must be fulfilled if the Ajax request is successful and be rejected otherwise.
 */
CampusExplorer.sendQuery = (query) => {
    // CampusExplorer.sendQueryPromise(query).then((responseString) => {
    //     return responseString
    // }).catch((err) => {
    //     console.log(err)
    // })
    return new Promise((resolve, reject) => {
        let xHttp = new XMLHttpRequest();
        let url = "http://localhost:4321/query";
        xHttp.onload = function() {
            if (this.status === 200) {
                resolve(this.response);
            } else if (this.status === 400) {
                reject(this.responseText);
            }
        };
        xHttp.onerror = function () {
            reject(this.responseText)
        }
        xHttp.open("POST", url, true);
        xHttp.setRequestHeader("Content-Type", "application/json");
        xHttp.send(JSON.stringify(query));
    });
};

// CampusExplorer.sendQueryPromise = (query) => {
//     return new Promise((resolve, reject) => {
//         let xHttp = new XMLHttpRequest();
//         let url = "http://localhost:4321/query";
//         xHttp.onload = function() {
//             resolve(this.responseText);
//         };
//         xHttp.onerror = function () {
//             reject(this.responseText)
//         }
//         xHttp.open("POST", url, true);
//         xHttp.setRequestHeader("Content-Type", "application/json");
//         xHttp.send(JSON.stringify(query));
//     });
// }
