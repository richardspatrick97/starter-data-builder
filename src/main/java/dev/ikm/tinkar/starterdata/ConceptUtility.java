package dev.ikm.tinkar.starterdata;

import dev.ikm.tinkar.entity.*;
import dev.ikm.tinkar.terms.EntityProxy;
import dev.ikm.tinkar.terms.TinkarTerm;

import java.util.UUID;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

class ConceptUtility {

    private static final Logger LOG = LoggerFactory.getLogger(ConceptUtility.class.getSimpleName());

    private final UUIDUtility uuidUtility;

    public ConceptUtility(UUIDUtility uuidUtility) {
        this.uuidUtility = uuidUtility;
    }

    protected Entity<? extends EntityVersion> createConcept(EntityProxy.Concept concept,
                                                            Entity<? extends EntityVersion> stampEntity){
        UUID uuid = concept.asUuidArray()[0];

        long[] additionalUuidLongsArr = null;
        if (concept.asUuidArray().length > 1) {
            additionalUuidLongsArr = new long[(concept.asUuidArray().length-1)*2];
            for (int i=1; i < concept.asUuidArray().length; i++) {
                additionalUuidLongsArr[(i*2)-2] = concept.asUuidArray()[i].getMostSignificantBits();
                additionalUuidLongsArr[(i*2)-1] = concept.asUuidArray()[i].getLeastSignificantBits();
            }
        }

        LOG.info("Building Concept");
        RecordListBuilder<ConceptVersionRecord> versions = RecordListBuilder.make();
        ConceptRecord conceptRecord = ConceptRecordBuilder.builder()
                .nid(concept.nid())
                .mostSignificantBits(uuid.getMostSignificantBits())
                .leastSignificantBits(uuid.getLeastSignificantBits())
                .additionalUuidLongs(additionalUuidLongsArr)
                .versions(versions.toImmutable())
                .build();

        versions.add(ConceptVersionRecordBuilder.builder()
                .chronology(conceptRecord)
                .stampNid(stampEntity.nid())
                .build());

        return ConceptRecordBuilder.builder(conceptRecord).versions(versions.toImmutable()).build();

    }
}
