import Map "mo:core/Map";
import Array "mo:core/Array";
import List "mo:core/List";
import Nat "mo:core/Nat";
import Order "mo:core/Order";
import Time "mo:core/Time";
import Runtime "mo:core/Runtime";

actor {
  type RoomType = {
    #openRandom;
    #career;
    #lost;
    #stress;
    #lateNight;
    #silentAccountability;
  };

  type RoomState = {
    id : Nat;
    roomType : RoomType;
    isActive : Bool;
    maxParticipants : Nat;
    participantCount : Nat;
    createdAt : Time.Time;
  };

  module RoomState {
    public func compare(room1 : RoomState, room2 : RoomState) : Order.Order {
      Nat.compare(room1.id, room2.id);
    };
  };

  let maxRoomsPerType = 10;

  let rooms = Map.empty<Nat, RoomState>();
  var nextRoomId : Nat = 0;

  func generateRoomId() : Nat {
    let currentId = nextRoomId;
    nextRoomId += 1;
    currentId;
  };

  public shared ({ caller }) func createRoom(roomType : RoomType) : async Nat {
    let existingRooms = getActiveRooms();
    if (existingRooms.size() >= maxRoomsPerType) {
      let roomsArray = existingRooms.toArray().sort();
      let lastRoom = roomsArray[(maxRoomsPerType - 1)];
      Runtime.trap("Room is full. Use room with ID: " # lastRoom.id.toText());
    };

    let roomId = generateRoomId();
    let room : RoomState = {
      id = roomId;
      roomType;
      isActive = true;
      maxParticipants = 8;
      participantCount = 0;
      createdAt = Time.now();
    };
    rooms.add(roomId, room);
    roomId;
  };

  func getActiveRooms() : List.List<RoomState> {
    rooms.values().filter(func(room) { room.isActive }).toList();
  };

  public shared ({ caller }) func addParticipant(roomId : Nat) : async () {
    switch (rooms.get(roomId)) {
      case (null) { Runtime.trap("Room does not exist") };
      case (?room) {
        if (not room.isActive) {
          Runtime.trap("Room is not active");
        };
        if (room.participantCount >= room.maxParticipants) {
          Runtime.trap("Room is full");
        };
        let updatedRoom : RoomState = {
          id = room.id;
          roomType = room.roomType;
          isActive = room.isActive;
          maxParticipants = room.maxParticipants;
          participantCount = room.participantCount + 1;
          createdAt = room.createdAt;
        };
        rooms.add(roomId, updatedRoom);
      };
    };
  };

  public shared ({ caller }) func removeParticipant(roomId : Nat) : async () {
    switch (rooms.get(roomId)) {
      case (null) { Runtime.trap("Room does not exist") };
      case (?room) {
        if (room.participantCount > 0) {
          let updatedRoom : RoomState = {
            id = room.id;
            roomType = room.roomType;
            isActive = room.isActive;
            maxParticipants = room.maxParticipants;
            participantCount = room.participantCount - 1;
            createdAt = room.createdAt;
          };
          rooms.add(roomId, updatedRoom);
        };
      };
    };
  };

  public query ({ caller }) func getRoomsByType(roomType : RoomType) : async [RoomState] {
    rooms.values().filter(func(room) { room.roomType == roomType and room.isActive }).toArray();
  };

  public shared ({ caller }) func getOrCreateRoom(roomType : RoomType) : async Nat {
    let existingRooms = getActiveRooms();

    let availableRooms = existingRooms.toArray().filter(
      func(room) { room.roomType == roomType and room.isActive and room.participantCount < room.maxParticipants }
    );

    if (availableRooms.size() > 0) {
      await addParticipant(availableRooms[0].id);
      availableRooms[0].id;
    } else {
      let newRoomId = await createRoom(roomType);
      await addParticipant(newRoomId);
      newRoomId;
    };
  };

  public shared ({ caller }) func leaveRoom(roomId : Nat) : async Bool {
    switch (rooms.get(roomId)) {
      case (null) { false };
      case (?room) {
        if (room.participantCount > 0) {
          await removeParticipant(roomId);
          true;
        } else { false };
      };
    };
  };

  public query ({ caller }) func getAllRooms() : async [RoomState] {
    getActiveRooms().toArray();
  };

  public query ({ caller }) func getRoomsByParticipants() : async [RoomState] {
    getActiveRooms().toArray();
  };

  public query ({ caller }) func getRoomCountByType(roomType : RoomType) : async Nat {
    getActiveRooms().toArray().filter(func(room) { room.roomType == roomType }).size();
  };
};
