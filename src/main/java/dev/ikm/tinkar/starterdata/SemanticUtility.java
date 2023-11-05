package dev.ikm.tinkar.starterdata;

import dev.ikm.tinkar.common.id.IntIdSet;
import dev.ikm.tinkar.common.id.IntIds;
import dev.ikm.tinkar.entity.*;
import dev.ikm.tinkar.terms.EntityProxy;
import dev.ikm.tinkar.terms.TinkarTerm;
import org.eclipse.collections.api.factory.Lists;
import org.eclipse.collections.api.list.MutableList;

import java.util.List;
import java.util.UUID;
import java.util.logging.Logger;

import static dev.ikm.tinkar.terms.TinkarTerm.DESCRIPTION_SEMANTIC;

class SemanticUtility {

    private static final Logger LOG = Logger.getLogger(SemanticUtility.class.getSimpleName());

    private final UUIDUtility uuidUtility;

    public SemanticUtility(UUIDUtility uuidUtility) {
        this.uuidUtility = uuidUtility;
    }

    protected Entity<? extends EntityVersion> createDescriptionSemantic(int referencedComponentNid,
                                                                        EntityProxy.Concept descriptionType,
                                                                        String text,
                                                                        Entity<? extends EntityVersion> authoringSTAMP){
        LOG.info("Building FQN Description Semantic: " + text);
        RecordListBuilder<SemanticVersionRecord> versions = RecordListBuilder.make();
        UUID descriptionSemanticUUID = uuidUtility.createUUID(text + "|" + DESCRIPTION_SEMANTIC.description());
        SemanticRecord semanticRecord = SemanticRecordBuilder.builder()
                .nid(EntityService.get().nidForUuids(descriptionSemanticUUID))
                .leastSignificantBits(descriptionSemanticUUID.getLeastSignificantBits())
                .mostSignificantBits(descriptionSemanticUUID.getMostSignificantBits())
                .additionalUuidLongs(null)
                .patternNid(TinkarTerm.DESCRIPTION_PATTERN.nid())
                .referencedComponentNid(referencedComponentNid)
                .versions(versions.toImmutable())
                .build();

        //Semantic Field Object values
        MutableList<Object> descriptionFields = Lists.mutable.empty();
        descriptionFields.add(TinkarTerm.ENGLISH_LANGUAGE);
        descriptionFields.add(text);
        descriptionFields.add(TinkarTerm.DESCRIPTION_NOT_CASE_SENSITIVE);
        descriptionFields.add(descriptionType);

        versions.add(SemanticVersionRecordBuilder.builder()
                .chronology(semanticRecord)
                .stampNid(authoringSTAMP.nid())
                .fieldValues(descriptionFields.toImmutable())
                .build());

        return SemanticRecordBuilder.builder(semanticRecord).versions(versions.toImmutable()).build();
    }

    protected Entity<? extends EntityVersion> createNavigationSemantic(int referencedComponentNid,
                                                                       EntityProxy.Pattern navigationPattern,
                                                                       List<EntityProxy.Concept> destinations,
                                                                       List<EntityProxy.Concept> origins,
                                                                       Entity<? extends EntityVersion> authoringSTAMP){
        LOG.info("Building " + navigationPattern.description() + " Navigation Semantic");
        RecordListBuilder<SemanticVersionRecord> versions = RecordListBuilder.make();
        UUID navigationSemanticUUID = uuidUtility.createUUID();
        SemanticRecord semanticRecord = SemanticRecordBuilder.builder()
                .nid(EntityService.get().nidForUuids(navigationSemanticUUID))
                .leastSignificantBits(navigationSemanticUUID.getLeastSignificantBits())
                .mostSignificantBits(navigationSemanticUUID.getMostSignificantBits())
                .additionalUuidLongs(null)
                .patternNid(navigationPattern.nid())
                .referencedComponentNid(referencedComponentNid)
                .versions(versions.toImmutable())
                .build();

        LOG.info("Building " + navigationPattern.description() + " Navigation Fields: " + "DES-" + destinations + " ORG-" + origins );
        MutableList<Object> navigationFields = Lists.mutable.empty();
        IntIdSet destinationIntIdSet;
        IntIdSet originIntIdSet;
        if (destinations == null ){
            destinationIntIdSet = IntIds.set.empty();
        } else {
            destinationIntIdSet = IntIds.set.of(destinations.stream().mapToInt(EntityProxy::nid).toArray());
        }
        if (origins == null){
            originIntIdSet = IntIds.set.empty();
        } else {
            originIntIdSet = IntIds.set.of(origins.stream().mapToInt(EntityProxy::nid).toArray());
        }
        navigationFields.add(destinationIntIdSet);
        navigationFields.add(originIntIdSet);

        versions.add(SemanticVersionRecordBuilder.builder()
                .chronology(semanticRecord)
                .stampNid(authoringSTAMP.nid())
                .fieldValues(navigationFields.toImmutable())
                .build());

        return SemanticRecordBuilder.builder(semanticRecord).versions(versions.toImmutable()).build();
    }

    protected Entity<? extends EntityVersion> createIdentifierSemantic(int referencedComponentNid,
                                                                       EntityProxy.Concept source,
                                                                       String id,
                                                                       Entity<? extends EntityVersion> authoringSTAMP){
        LOG.info("Building Identifier Semantic");
        RecordListBuilder<SemanticVersionRecord> versions = RecordListBuilder.make();
        UUID navigationSemanticUUID = uuidUtility.createUUID();
        SemanticRecord semanticRecord = SemanticRecordBuilder.builder()
                .nid(EntityService.get().nidForUuids(navigationSemanticUUID))
                .leastSignificantBits(navigationSemanticUUID.getLeastSignificantBits())
                .mostSignificantBits(navigationSemanticUUID.getMostSignificantBits())
                .additionalUuidLongs(null)
                .patternNid(StarterData.identifierPattern.nid())
                .referencedComponentNid(referencedComponentNid)
                .versions(versions.toImmutable())
                .build();

        LOG.info("Building Identifier Pattern Fields");
        MutableList<Object> identifierFields = Lists.mutable.empty();
        identifierFields.add(source);
        identifierFields.add(id);

        versions.add(SemanticVersionRecordBuilder.builder()
                .chronology(semanticRecord)
                .stampNid(authoringSTAMP.nid())
                .fieldValues(identifierFields.toImmutable())
                .build());

        return SemanticRecordBuilder.builder(semanticRecord).versions(versions.toImmutable()).build();
    }

    protected Entity<? extends EntityVersion> createDialectSemantic(int referencedComponentNid,
                                                                    EntityProxy.Concept dialect,
                                                                    Entity<? extends EntityVersion> authoringSTAMP){
        LOG.info("Building Dialect Semantic");
        RecordListBuilder<SemanticVersionRecord> versions = RecordListBuilder.make();
        UUID dialectSemanticUUID = uuidUtility.createUUID();
        SemanticRecord semanticRecord = SemanticRecordBuilder.builder()
                .nid(EntityService.get().nidForUuids(dialectSemanticUUID))
                .leastSignificantBits(dialectSemanticUUID.getLeastSignificantBits())
                .mostSignificantBits(dialectSemanticUUID.getMostSignificantBits())
                .additionalUuidLongs(null)
                .patternNid(TinkarTerm.US_DIALECT_PATTERN.nid())
                .referencedComponentNid(referencedComponentNid)
                .versions(versions.toImmutable())
                .build();

        LOG.info("Building Dialect Pattern Fields");
        MutableList<Object> dialectFields = Lists.mutable.empty();
        dialectFields.add(dialect);

        versions.add(SemanticVersionRecordBuilder.builder()
                .chronology(semanticRecord)
                .stampNid(authoringSTAMP.nid())
                .fieldValues(dialectFields.toImmutable())
                .build());

        return SemanticRecordBuilder.builder(semanticRecord).versions(versions.toImmutable()).build();
    }

    protected Entity<? extends EntityVersion> createAxiomSyntax(int referencedComponentNid,
                                                                String axiomSyntax,
                                                                Entity<? extends EntityVersion> authoringSTAMP){
        LOG.info("Building Axiom Syntax Semantic");
        RecordListBuilder<SemanticVersionRecord> versions = RecordListBuilder.make();
        UUID axiomSyntaxSemantic = uuidUtility.createUUID();
        SemanticRecord semanticRecord = SemanticRecordBuilder.builder()
                .nid(EntityService.get().nidForUuids(axiomSyntaxSemantic))
                .leastSignificantBits(axiomSyntaxSemantic.getLeastSignificantBits())
                .mostSignificantBits(axiomSyntaxSemantic.getMostSignificantBits())
                .additionalUuidLongs(null)
                .patternNid(StarterData.axiomSyntaxPattern.nid())
                .referencedComponentNid(referencedComponentNid)
                .versions(versions.toImmutable())
                .build();

        LOG.info("Building AxiomSyntax Pattern Fields");
        MutableList<Object> axiomSyntaxFields = Lists.mutable.empty();
        axiomSyntaxFields.add(axiomSyntax);

        versions.add(SemanticVersionRecordBuilder.builder()
                .chronology(semanticRecord)
                .stampNid(authoringSTAMP.nid())
                .fieldValues(axiomSyntaxFields.toImmutable())
                .build());

        return SemanticRecordBuilder.builder(semanticRecord).versions(versions.toImmutable()).build();
    }

}
