import {Room, TableKind} from "./IInsightFacade";
import {isIterable} from "./AddRemoveListDatasetsHelper";

export function getChildNodeWithGivenName(parentNode: any, childNodeName: string): any {
    if (!parentNode) {
        return null;
    }
    for (let childNodeKey in parentNode.childNodes) {
        let childNode = parentNode.childNodes[childNodeKey];
        if (childNode.nodeName === childNodeName) {
            return childNode;
        }
    }
    return null;
}

export function isTableKind(tableNode: any, tableKind: TableKind): boolean {
    // TODO: Think about find the child's child nodes with thead name, because it may not be in the direct child but
    //  only search x amount for runtime
    let tableHeadNode: any = this.getChildNodeWithGivenName(tableNode, "thead");
    if (tableHeadNode == null) {
        return false;
    }
    let tableHeadsTrNode: any = this.getChildNodeWithGivenName(tableHeadNode, "tr");
    if (tableHeadsTrNode == null) {
        return false;
    }
    for (let thNodeKey in tableHeadsTrNode.childNodes) {
        let thNode = tableHeadsTrNode.childNodes[thNodeKey];
        if (thNode.nodeName === "th" && Object.keys(thNode).includes("childNodes")) {
            for (let thNodeChild of thNode.childNodes) {
                let thNodeVal = thNodeChild.value;
                if (thNodeVal.includes("Building") && tableKind === TableKind.Building) {
                    return true;
                }
                if (thNodeVal.includes("Room") && tableKind === TableKind.Room) {
                    return true;
                }
            }
        }
    }
    return false;
}

export function validateTableKind(tableNode: any, tableKindToGet: TableKind): boolean {
    if (tableKindToGet === TableKind.Building && this.isTableKind(tableNode, tableKindToGet)) {
        return true;
    } else if (tableKindToGet === TableKind.Room && this.isTableKind(tableNode, tableKindToGet)) {
        return true;
    }
    return false;
}

export function getNodeWithGivenName(htmlTreeNode: any, nodeNameToGet: string, tableNodeToGet?: TableKind): any {
    if (!htmlTreeNode) {
        return null;
    }
    let stack: any[] = [];
    stack.push(htmlTreeNode);
    while (stack.length > 0) {
        let node = stack.pop();
        if (node.nodeName === nodeNameToGet) {
            if (tableNodeToGet && this.validateTableKind(node, tableNodeToGet)) {
                return node;
            } else if (!tableNodeToGet) {
                return node;
            }
        } else {
            if (("childNodes" in node) && isIterable(node.childNodes)) {
                for (let childNodeKey in node.childNodes) {
                    let childNode = node.childNodes[childNodeKey];
                    stack.push(childNode);
                }
            }
        }
    }
    return null;
}

export function storeBuildingInfo(buildingInfo: any, buildingInfoFromIndexFile: any): void {
    let code = buildingInfo.code.trim();
    let address = buildingInfo.address.trim();
    let name = buildingInfo.name.trim();
    let href = buildingInfo.href.trim();
    buildingInfoFromIndexFile[href] = {name: name, code: code, address: address};
}

export function getBuildingInfoFromTdNode(tdNode: any, buildingInfo: any): any {
    if (tdNode && ("attrs" in tdNode) && isIterable(tdNode.attrs)) {
        for (let tdNodeAttr of tdNode.attrs) {
            if (tdNodeAttr.value === "views-field views-field-field-building-code") {
                let tdNodeChildNode = this.getChildNodeWithGivenName(tdNode, "#text");
                buildingInfo.code = (tdNodeChildNode ? tdNodeChildNode.value : "");
            } else if (tdNodeAttr.value === "views-field views-field-field-building-address") {
                let tdNodeChildNode = this.getChildNodeWithGivenName(tdNode, "#text");
                buildingInfo.address = (tdNodeChildNode ? tdNodeChildNode.value : "");
            } else if (tdNodeAttr.value === "views-field views-field-title") {
                let nodeA = this.getChildNodeWithGivenName(tdNode, "a");
                if (nodeA && ("childNodes" in nodeA) && ("attrs" in nodeA)) {
                    for (let nodeAAttr of nodeA.attrs) {
                        if (nodeAAttr.name === "href") {
                            buildingInfo.href = nodeAAttr.value;
                        }
                    }
                    let nodeAChild = this.getChildNodeWithGivenName(nodeA, "#text");
                    buildingInfo.name = (nodeAChild ? nodeAChild.value : "");
                }
            }
        }
    }
    return buildingInfo;
}

export function getBuildingInfoFromTableNode(buildingsTableNode: any) {
    let tableBodyNode: any = this.getChildNodeWithGivenName(buildingsTableNode, "tbody");
    let buildingInfoFromIndexFile: {[href: string]: {name: string, code: string, address: string}} = {};
    if (tableBodyNode && ("childNodes" in tableBodyNode)) {
        for (let trNode of tableBodyNode.childNodes) {
            if (trNode.nodeName === "tr" && ("childNodes" in trNode)) {
                let buildingInfo = {name: "", code: "", address: "", href: ""};
                for (let tdNode of trNode.childNodes) {
                    if (tdNode.nodeName === "td" && ("childNodes" in tdNode) && ("attrs" in tdNode)) {
                        buildingInfo = this.getBuildingInfoFromTdNode(tdNode, buildingInfo);
                    }
                }
                if (buildingInfo.name && buildingInfo.code && buildingInfo.address && buildingInfo.href) {
                    this.storeBuildingInfo(buildingInfo, buildingInfoFromIndexFile);
                }
            }
        }
    }
    return buildingInfoFromIndexFile;
}

export function assignRemainingRoomKeys(trNode: any, room: Room, buildingCode: string) {
    for (let tdNode of trNode.childNodes) {
        if (tdNode.nodeName === "td" && ("attrs" in tdNode)) {
            for (let tdNodeAttr of tdNode.attrs) {
                // TODO: Check with TA on how I would not hard code the values here?
                if (tdNodeAttr.value === "views-field views-field-field-room-number") {
                    for (let tdNodeChild of tdNode.childNodes) {
                        if (tdNodeChild.nodeName === "a") {
                            let nodeA = tdNodeChild;
                            for (let nodeAAttr of nodeA.attrs) {
                                if (nodeAAttr.name === "href") {
                                    room.href = String(nodeAAttr.value);
                                }
                            }
                            let childNodeOfA = this.getChildNodeWithGivenName(nodeA, "#text");
                            if (childNodeOfA) {
                                room.number = String(childNodeOfA.value).trim();
                                room.name = String(buildingCode + "_" + room.number);
                            }
                        }
                    }
                } else if (tdNodeAttr.value === "views-field views-field-field-room-capacity") {
                    room.seats = Number(this.getChildNodeWithGivenName(tdNode, "#text").value.trim());
                } else if (tdNodeAttr.value === "views-field views-field-field-room-furniture") {
                    room.furniture = String(this.getChildNodeWithGivenName(tdNode, "#text").value).trim();
                } else if (tdNodeAttr.value === "views-field views-field-field-room-type") {
                    room.type = String(this.getChildNodeWithGivenName(tdNode, "#text").value).trim();
                }
            }
        }
    }
}
