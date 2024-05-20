package dev.ikm.tinkar.starterdata;

import dev.ikm.tinkar.entity.*;
import dev.ikm.tinkar.terms.EntityProxy;

import java.util.UUID;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

class STAMPUtility {

    private static final Logger LOG = LoggerFactory.getLogger(STAMPUtility.class.getSimpleName());

    private final UUIDUtility uuidUtility;

    public STAMPUtility(UUIDUtility uuidUtility) {
        this.uuidUtility = uuidUtility;
    }

    protected Entity<? extends EntityVersion> createSTAMP(EntityProxy.Concept status, long time, EntityProxy.Concept author, EntityProxy.Concept module, EntityProxy.Concept path){
        LOG.info("Building STAMP Chronology");
        UUID stampUUID = uuidUtility.createUUID(
                status.description() + "|" +
                        time + "|" +
                        author.description() + "|" +
                        module.description() + "|" +
                        path.description());
        RecordListBuilder<StampVersionRecord> versions = RecordListBuilder.make();
        //Create STAMP Chronology
        StampRecord stampRecord = StampRecordBuilder.builder()
                .nid(EntityService.get().nidForUuids(stampUUID))
                .leastSignificantBits(stampUUID.getLeastSignificantBits())
                .mostSignificantBits(stampUUID.getMostSignificantBits())
                .additionalUuidLongs(null)
                .versions(versions)
                .build();

        LOG.info("Building STAMP Version");
        //Create STAMP Version
        versions.add(StampVersionRecordBuilder.builder()
                .chronology(stampRecord)
                .stateNid(status.nid())
                .time(time)
                .authorNid(author.nid())
                .moduleNid(module.nid())
                .pathNid(path.nid())
                .build());

        return StampRecordBuilder.builder(stampRecord).versions(versions.toImmutable()).build();
    }
}
