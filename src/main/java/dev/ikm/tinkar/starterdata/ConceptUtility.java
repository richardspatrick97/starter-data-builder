package dev.ikm.tinkar.starterdata;

import dev.ikm.tinkar.entity.*;
import dev.ikm.tinkar.terms.EntityProxy;
import dev.ikm.tinkar.terms.TinkarTerm;

import java.util.UUID;
import java.util.logging.Logger;

class ConceptUtility {

    private static final Logger LOG = Logger.getLogger(ConceptUtility.class.getSimpleName());

    private final UUIDUtility uuidUtility;

    public ConceptUtility(UUIDUtility uuidUtility) {
        this.uuidUtility = uuidUtility;
    }

    protected Entity<? extends EntityVersion> createConcept(EntityProxy.Concept concept,
                                                            Entity<? extends EntityVersion> stampEntity){
        UUID uuid = concept.asUuidArray()[0];
        if (concept.equals(TinkarTerm.HEALTH_CONCEPT)) {
            uuid = concept.asUuidArray()[1];
        }
        LOG.info("Building Concept");
        RecordListBuilder<ConceptVersionRecord> versions = RecordListBuilder.make();
        ConceptRecord conceptRecord = ConceptRecordBuilder.builder()
                .nid(concept.nid())
                .leastSignificantBits(uuid.getLeastSignificantBits())
                .mostSignificantBits(uuid.getMostSignificantBits())
                .additionalUuidLongs(null)
                .versions(versions.toImmutable())
                .build();

        versions.add(ConceptVersionRecordBuilder.builder()
                .chronology(conceptRecord)
                .stampNid(stampEntity.nid())
                .build());

        return ConceptRecordBuilder.builder(conceptRecord).versions(versions.toImmutable()).build();

    }
}
