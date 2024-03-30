import Text "mo:base/Text";
import Nat "mo:base/Nat";
import Bool "mo:base/Bool";
import HashMap "mo:base/HashMap";
import Buffer "mo:base/Buffer";
import Int "mo:base/Int";

actor
{
  // Donor and Recipient are the two types of actors in the system.
  public type Donor = 
  {
    choiceVisible:Bool;
    name:Text;
    surname:Text;
    donation:Nat;
    message:?Text;
  };

  public type Recipient = 
  {
    choiceVisible:Bool;
    name:Text;
    surname:Text;
    need:Nat;

  };

  public type Donation = 
  {
    id:Int;
    amount:Nat;

  };
  
  var required : Buffer.Buffer<Nat> = Buffer.Buffer<Nat>(0);
  var donors: HashMap.HashMap<Text, Donor> = HashMap.HashMap<Text, Donor>(0, Text.equal, Text.hash);
  var recipients: HashMap.HashMap<Text, Recipient> = HashMap.HashMap<Text, Recipient>(0, Text.equal, Text.hash);
  var donations: HashMap.HashMap<Text, Donation> = HashMap.HashMap<Text, Donation>(0, Text.equal, Text.hash);
  var messages:Buffer.Buffer<?Text> = Buffer.Buffer<?Text>(0);
  var totalDonations:Nat = 0;
  var totalNeeds:Nat = 0;
  var totalDonationsSoFar:Nat = 0;
  private stable var donationId:Nat = 0;

  // Heartbeat function to advance the system at regular intervals.
  system func heartbeat(): async ()
  {
    await progressS();
  };

  // Function to register a donor, adding a new donor to the system.
  public func registerDonor(choiceVisible:Bool,name:Text, surname:Text, donation:Nat, message:?Text): async ()
  {
    // Behavior differs based on donor visibility.
    if (choiceVisible == true)
    {
      // Create a visible donor.
      let visibleDonor:Donor = {choiceVisible=choiceVisible; name=name; surname=surname; donation=donation; message=message};
      switch (donors.get(name)) {
        case (null) {
          donors.put(name, visibleDonor);
          totalDonations := totalDonations + donation;
          if(message != null)
          {
            messages.add(visibleDonor.message)
          }
        };
        case (_) {
          return ();
        };
      };
    }
    else{
      // Create an invisible donor.
      let invisibleDonor:Donor = {choiceVisible=choiceVisible; name=""; surname=""; donation=donation; message=message};
      switch (donors.get(name)) {
        case (null) {
          donors.put(name, invisibleDonor);
          totalDonations := totalDonations + donation;
          if(message != null)
          {
            messages.add(invisibleDonor.message)
          }
        };
        case (_) {
          return ();
        };
      };
    }
  };

  // Function to register a recipient, adding a new recipient to the system.
  public func registerRecipient(choiceVisible:Bool,name:Text, surname:Text, need:Nat): async ?Text
  {
    if(choiceVisible == true){
      // Create a visible recipient.
      let visibleRecipient:Recipient = {choiceVisible=choiceVisible; name=name; surname=surname; need=need;};
      switch (recipients.get(name)) {
        case (null) {
          if(totalDonations >= need){
            totalDonations := totalDonations - need;
            totalDonationsSoFar := totalDonationsSoFar + need;         
            let donate:Donation = {id=donationId; amount=need};
            donations.put(Nat.toText(donationId),donate);
            donationId := donationId + 1;
            if(messages.size() >= 1){
              let x =messages.remove(0);
              return x;
            }
            else{
              return null;
            }
          }
          else{
            totalNeeds := totalNeeds + need;
            recipients.put(name, visibleRecipient);
            required.add(need);
            return null;
          }
        };
        case (_) {
          return null;
        };
      };
    }
    else{
      // Create an invisible recipient.
      let invisibleRecipient:Recipient = {choiceVisible=choiceVisible; name=""; surname=""; need=need;};
      switch (recipients.get(name)) {
        case (null) {
          if(totalDonations >= need){
            totalDonations := totalDonations - need;
            totalDonationsSoFar := totalDonationsSoFar + need;
            let donate:Donation = {id=donationId; amount=need};
            donations.put(Nat.toText(donationId),donate);
            donationId := donationId + 1;
            if(messages.size()>=1){
              let x =messages.remove(0);
              return x;
            }
            else{
              return null;
            }
          }
          else{
            totalNeeds := totalNeeds + need;
            recipients.put(name, invisibleRecipient);
            required.add(need);
            return null;
          }
        };
        case (_) {
          return null;
        };
      };
    }  
  };
  
  // Query to get the total donation amount.
  public query func getTotalDonate() :async  Nat {
    return totalDonations;
  };

  // Query to get the total needed amount.
  public query func getTotalNeed() :async  Nat {
    return totalNeeds;
  };

  // Query to get the donor with the specified name.
  public query func getDonor(name:Text) :async  Donor {
    let maybeDonor = donors.get(name);
    switch (maybeDonor) {
        case (?donor) {
            return donor;
        };
        case (null) {
            return {choiceVisible=false; name = ""; surname = ""; donation = 0; message=null};
        };
    };
  };

  // Query to get the recipient with the specified name.
  public query func getRecipient(name:Text) :async  Recipient {
    let maybeRecipient = recipients.get(name);
    switch (maybeRecipient) {
        case (?recipient) {
            return recipient;
        };
        case (null) {
            return {choiceVisible=false;name = ""; surname = ""; need = 0};
        };
    };
  };

  // Private function to accept a request from a recipient.
  private func acceptRequest(recipient:Recipient) : async () {
    if(totalDonations >= totalNeeds){
      totalDonations := totalDonations - recipient.need;
      totalNeeds := totalNeeds - recipient.need;
      totalDonationsSoFar := totalDonationsSoFar + recipient.need;
      let x =required.remove(0);
      recipients.delete(recipient.name);
      let donate:Donation = {id=donationId; amount=recipient.need};
      donations.put(Nat.toText(donationId),donate);
      donationId +=  1;
    }
    else{
    return ();
    }; 
  };

  // Private function to progress the system.
  private func progressS():async ()
  {
    if (totalNeeds > 0)
    {
      let namereci:?Text = recipients.keys().next();
      let text : Text = switch (namereci) {
            case (null) { "default" }; 
            case (?t) { t }; 
          };
      var recipient = recipients.get(text);
      let reci:Recipient = switch(recipient){
        case (?r) { r };
        case (null) { {choiceVisible=false;name = ""; surname = ""; need = 0} };
      };

      if (recipient != null)
      {
        let x = await acceptRequest(reci);
        x
      }
    };
    for(i in recipients.keys()){
      let recipient:Recipient = switch(recipients.get(i)){
        case (?r) { r };
        case (null) { {choiceVisible=false;name = ""; surname = ""; need = 0}};
      };
      for (j in donors.keys()){
        let donor = switch(donors.get(j)){
          case (?d) { d };
          case (null) { {choiceVisible=false; name = ""; surname = ""; donation = 0; message=null} };
        };
        if(recipient.need != 0 and donor.donation != 0){
          if(recipient.need <= donor.donation){
            totalDonations := totalDonations - recipient.need;
            totalNeeds := totalNeeds - recipient.need;
            totalDonationsSoFar := totalDonationsSoFar + recipient.need;
            let x =required.remove(0);
            recipients.delete(i);
            donors.delete(j);
            let donate:Donation = {id=donationId; amount=recipient.need};
            donations.put(Nat.toText(donationId),donate);
            donationId +=  1;
          }
        }
      };
    }; 
    for(k in recipients.keys()){
      let recipient:Recipient = switch(recipients.get(k)){
        case (?r) { r };
        case (null) { {choiceVisible=false;name = ""; surname = ""; need = 0}};
      };
      if(totalDonations>=recipient.need){
        totalDonations := totalDonations - recipient.need;
        totalNeeds := totalNeeds - recipient.need;
        totalDonationsSoFar := totalDonationsSoFar + recipient.need;
        let x =required.remove(0);
        recipients.delete(k);
        let donate:Donation = {id=donationId; amount=recipient.need};
        donations.put(Nat.toText(donationId),donate);
        donationId +=  1;
      }
    };

  };
  // Query to get the total donation amount so far.
  public query func amountDonationSoFar() : async Nat {
    return totalDonationsSoFar;
  };
  
  // Query to get the donation history.
  public query func getDonationHistory() : async [Donation] {
     let donationsT:Buffer.Buffer<Donation> = Buffer.Buffer<Donation>(0);
    for (donation in donations.vals()) {
      donationsT.add(donation);
    };
    return donationsT.toArray();
  };

  // Query to get the donation with the specified id.
  public query func getDonation(id:Nat) : async Donation {
    let donation = donations.get(Nat.toText(id));
    switch (donation) {
        case (?donation) {
            return donation;
        };
        case (null) {
            return {id=id; amount=0};
        };
    };
  };

}