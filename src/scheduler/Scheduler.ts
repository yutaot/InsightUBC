import {IScheduler, SchedRoom, SchedSection, TimeSlot} from "./IScheduler";

export default class Scheduler implements IScheduler {

    public schedule(sections: SchedSection[], rooms: SchedRoom[]): Array<[SchedRoom, SchedSection, TimeSlot]> {
        let totalEnrolment = 0;
        for (let section of sections) {
            totalEnrolment += section.courses_audit + section.courses_pass + section.courses_fail;
        }
        let maxScore = 0;
        let bestSchedule: any = [];
        let sortedSections = sections.sort((a, b) =>
            (a.courses_pass + a.courses_fail + a.courses_audit < b.courses_pass + b.courses_fail + b.courses_audit) ? 1
                : ((b.courses_pass + b.courses_fail + b.courses_audit <
                a.courses_pass + a.courses_fail + a.courses_audit) ? -1 : 0));
        let biggestToSmallestRoom = rooms.sort((a, b) =>
            (a.rooms_seats < b.rooms_seats) ? 1 : ((b.rooms_seats < a.rooms_seats) ? -1 : 0));
        let room = biggestToSmallestRoom[0];
        let selectedRooms: any = [room];
        let schedule = this.createSchedule(sortedSections, selectedRooms);
        let score = this.getScore(schedule, totalEnrolment, room, room);
        if (score > maxScore) {
            maxScore = score;
            bestSchedule = schedule;
        }
        // TODO: to make code efficient, calculate distance from room to all other rooms before the loop
        for (let i = 1; i < rooms.length; i++) {
            let nextClosestRoom = this.findNextClosestRoom(room, selectedRooms, rooms);
            selectedRooms.push(nextClosestRoom);
            let marginalSchedule = this.createSchedule(sections, selectedRooms);
            let marginalScore = this.getScore(marginalSchedule, totalEnrolment, room, nextClosestRoom);
            if (marginalScore > maxScore) {
                maxScore = marginalScore;
                bestSchedule = marginalSchedule;
            }
        }
        // for (let room of rooms) {
        //     let selectedRooms: any = [room];
        //     let schedule = this.createSchedule(sortedSections, selectedRooms);
        //     let score = this.getScore(schedule, totalEnrolment, room, room);
        //     if (score > maxScore) {
        //         maxScore = score;
        //         bestSchedule = schedule;
        //     }
        //     // TODO: to make code efficient, calculate distance from room to all other rooms before the loop
        //     for (let i = 1; i < rooms.length; i++) {
        //         let nextClosestRoom = this.findNextClosestRoom(room, selectedRooms, rooms);
        //         selectedRooms.push(nextClosestRoom);
        //         let marginalSchedule = this.createSchedule(sections, selectedRooms);
        //         let marginalScore = this.getScore(marginalSchedule, totalEnrolment, room, nextClosestRoom);
        //         if (marginalScore > maxScore) {
        //             maxScore = marginalScore;
        //             bestSchedule = marginalSchedule;
        //         }
        //     }
        // }
        return bestSchedule;
    }

    private createSchedule(sections: SchedSection[], selectedRooms: any[]): any {
        let schedule: any = [];
        let sortedRooms = selectedRooms.sort((a, b) =>
            (a.rooms_seats < b.rooms_seats) ? 1 : ((b.rooms_seats < a.rooms_seats) ? -1 : 0));
        let roomsWithTimeslot: any = [];
        for (let room of sortedRooms) {
            let newRoomObject: { room: any; timeSlotObj: any };
            newRoomObject = {
                room: room,
                timeSlotObj: {
                    "MWF 0800-0900" : 0,
                    "MWF 0900-1000" : 0,
                    "MWF 1000-1100" : 0,
                    "MWF 1100-1200" : 0,
                    "MWF 1200-1300" : 0,
                    "MWF 1300-1400" : 0,
                    "MWF 1400-1500" : 0,
                    "MWF 1500-1600" : 0,
                    "MWF 1600-1700" : 0,
                    "TR  0800-0930" : 0,
                    "TR  0930-1100" : 0,
                    "TR  1100-1230" : 0,
                    "TR  1230-1400" : 0,
                    "TR  1400-1530" : 0,
                    "TR  1530-1700" : 0}
            };
            roomsWithTimeslot.push(newRoomObject);
        }
        let sectionScheduledForSameClass: any = [];
        let groupByArr = this.groupByClass(sections);
        let tripleArr = this.matchScheduleForSameClass(schedule, sortedRooms, roomsWithTimeslot, groupByArr,
            sectionScheduledForSameClass);
        schedule = tripleArr[0];
        roomsWithTimeslot = tripleArr[1];
        sectionScheduledForSameClass = tripleArr[2];
        let sortedSectionsRefined = sections.filter((x) => !sectionScheduledForSameClass.includes(x));
        return this.matchScheduleForNonSameClass(schedule, sortedSectionsRefined, sortedRooms, roomsWithTimeslot);
    }

    private matchScheduleForSameClass(schedule: any[], sortedRooms: any[], roomsWithTimeslot: any[], groupByArr: any[],
                                      sectionScheduledForSameClass: any[]): any {
        for (let course of groupByArr) {
            if (course.sectionArr.length > 1) {
                let timeSlotList: any = { "MWF 0800-0900" : 0, "MWF 0900-1000" : 0, "MWF 1000-1100" : 0,
                    "MWF 1100-1200" : 0, "MWF 1200-1300" : 0, "MWF 1300-1400" : 0, "MWF 1400-1500" : 0,
                    "MWF 1500-1600" : 0, "MWF 1600-1700" : 0, "TR  0800-0930" : 0, "TR  0930-1100" : 0,
                    "TR  1100-1230" : 0, "TR  1230-1400" : 0, "TR  1400-1530" : 0,
                    "TR  1530-1700" : 0};
                for (let section of course.sectionArr) {
                    let sectionSize = section.courses_pass + section.courses_fail + section.courses_audit;
                    sectionScheduledForSameClass.push(section);
                    for (let i = 0; i < sortedRooms.length; i++) {
                        let outerMatched = false;
                        if (i === 0 && sortedRooms[0].rooms_seats < sectionSize) {
                            break;
                        }
                        if (sortedRooms[i].rooms_seats >= sectionSize) {
                            let timeSlotArr: any = [];
                            for (let timeSlot in roomsWithTimeslot[i].timeSlotObj) {
                                let time = timeSlot.toString();
                                if (roomsWithTimeslot[i].timeSlotObj[time] === 0 && timeSlotList[time] === 0) {
                                    timeSlotArr.push(sortedRooms[i]);
                                    timeSlotArr.push(section);
                                    timeSlotArr.push(time);
                                    roomsWithTimeslot[i].timeSlotObj[time] = 1;
                                    timeSlotList[time] = 1;
                                    schedule.push(timeSlotArr);
                                    outerMatched = true;
                                    break;
                                }
                            }
                        }
                        if (outerMatched === true) {
                            break;
                        }
                    }
                }
            }
        }
        return [schedule, roomsWithTimeslot, sectionScheduledForSameClass];
    }

    private matchScheduleForNonSameClass(schedule: any[], sortedSectionsRefined: any[],
                                         sortedRooms: any[], roomsWithTimeslot: any[]): any {
        for (let section of sortedSectionsRefined) {
            let sectionSize = section.courses_pass + section.courses_fail + section.courses_audit;
            for (let i = 0; i < sortedRooms.length; i++) {
                let outerMatched = false;
                if (i === 0 && sortedRooms[0].rooms_seats < sectionSize) {
                    break;
                }
                if (sortedRooms[i].rooms_seats >= sectionSize) {
                    let timeSlotArr: any = [];
                    for (let timeSlot in roomsWithTimeslot[i].timeSlotObj) {
                        let time = timeSlot.toString();
                        if (roomsWithTimeslot[i].timeSlotObj[time] === 0) {
                            timeSlotArr.push(sortedRooms[i]);
                            timeSlotArr.push(section);
                            timeSlotArr.push(time);
                            roomsWithTimeslot[i].timeSlotObj[time] = 1;
                            schedule.push(timeSlotArr);
                            outerMatched = true;
                            break;
                        }
                    }
                }
                if (outerMatched === true) {
                    break;
                }
            }
        }
        return schedule;
    }

    // TODO: make a data structure of an array that has objects where objects have class name and section arr
    private groupByClass(sections: SchedSection[]): any[] {
        let groupByArr: any = [];
        for (let section of sections) {
            let matched = false;
            for (let obj of groupByArr) {
                if (obj.class === section.courses_dept + section.courses_id) {
                    obj.sectionArr.push(section);
                    matched = true;
                    break;
                }
            }
            if (!matched) {
                let newGroupObject: { class: string; sectionArr: any[] };
                newGroupObject = {
                    class: section.courses_dept + section.courses_id,
                    sectionArr: [section]
                };
                groupByArr.push(newGroupObject);
            }
        }
        return groupByArr;
    }

    private getScore(schedule: any, totalEnrolment: number, centerRoom: any, farthestRoom: any): number {
        let e;
        let scheduledEnrolment = 0;
        let d;
        if (schedule.length === 0) {
            return Number.NEGATIVE_INFINITY;
        }
        // each validSection is an array
        for (let validSection of schedule) {
            let sectionE = validSection[1].courses_fail + validSection[1].courses_pass + validSection[1].courses_audit;
            scheduledEnrolment += sectionE;
        }
        e = scheduledEnrolment / totalEnrolment;
        if (centerRoom === farthestRoom) {
            d = 0;
        } else {
            let distance = this.calculateDistance(centerRoom, farthestRoom);
            d = distance / 1372;
        }
        return (0.7 * e) + (0.3 * (1 - d));
    }

    // make the closest one the closest to the middle node
    private findNextClosestRoom(room: any, selectedRooms: any[], rooms: SchedRoom[]): any {
        // for (let i = 0; i < distanceArr.length; i++) {
        //
        // }
        let difference = rooms.filter((x) => !selectedRooms.includes(x));
        let minDistance = Infinity;
        let nextClosestRoom;
        for (let outerRoom of difference) {
            let distance = this.calculateDistance(room, outerRoom);
            if (distance < minDistance) {
                minDistance = distance;
                nextClosestRoom = outerRoom;
            }
        }
        return nextClosestRoom;
    }

    private calculateDistance(room1: any, room2: any): number {
        let lat1 = room1.rooms_lat;
        let lon1 = room1.rooms_lon;
        let lat2 = room2.rooms_lat;
        let lon2 = room2.rooms_lon;

        const R = 6371e3; // metres
        const φ1 = lat1 * Math.PI / 180; // φ, λ in radians
        const φ2 = lat2 * Math.PI / 180;
        const Δφ = (lat2 - lat1) * Math.PI / 180;
        const Δλ = (lon2 - lon1) * Math.PI / 180;

        const a = Math.sin(Δφ / 2) * Math.sin(Δφ / 2) +
            Math.cos(φ1) * Math.cos(φ2) *
            Math.sin(Δλ / 2) * Math.sin(Δλ / 2);
        const c = 2 * Math.atan2(Math.sqrt(a), Math.sqrt(1 - a));

        const d = R * c; // in metres
        return d;
    }
}
