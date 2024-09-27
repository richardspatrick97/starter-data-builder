package dev.ikm.tinkar.starterdata;

import dev.ikm.tinkar.common.id.IntIdSet;
import dev.ikm.tinkar.common.id.IntIds;
import dev.ikm.tinkar.entity.Entity;
import dev.ikm.tinkar.entity.EntityService;
import dev.ikm.tinkar.entity.EntityVersion;
import dev.ikm.tinkar.entity.RecordListBuilder;
import dev.ikm.tinkar.entity.SemanticRecord;
import dev.ikm.tinkar.entity.SemanticRecordBuilder;
import dev.ikm.tinkar.entity.SemanticVersionRecord;
import dev.ikm.tinkar.entity.SemanticVersionRecordBuilder;
import dev.ikm.tinkar.entity.graph.DiTreeEntity;
import dev.ikm.tinkar.entity.graph.EntityVertex;
import dev.ikm.tinkar.terms.ConceptFacade;
import dev.ikm.tinkar.terms.EntityProxy;
import dev.ikm.tinkar.terms.TinkarTerm;
import org.eclipse.collections.api.factory.Lists;
import org.eclipse.collections.api.factory.primitive.IntIntMaps;
import org.eclipse.collections.api.factory.primitive.IntLists;
import org.eclipse.collections.api.factory.primitive.IntObjectMaps;
import org.eclipse.collections.api.list.MutableList;
import org.eclipse.collections.api.list.primitive.ImmutableIntList;
import org.eclipse.collections.api.list.primitive.MutableIntList;
import org.eclipse.collections.api.map.primitive.MutableIntIntMap;
import org.eclipse.collections.api.map.primitive.MutableIntObjectMap;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.util.List;
import java.util.UUID;

import static dev.ikm.tinkar.entity.RecordListBuilder.make;
import static dev.ikm.tinkar.entity.graph.EntityVertex.abstractObject;

class SemanticUtility {

    private static final Logger LOG = LoggerFactory.getLogger(SemanticUtility.class.getSimpleName());

    private final UUIDUtility uuidUtility;

    public SemanticUtility(UUIDUtility uuidUtility) {
        this.uuidUtility = uuidUtility;
    }

    protected Entity<? extends EntityVersion> createDescriptionSemantic(int referencedComponentNid,
                                                                        EntityProxy.Concept descriptionType,
                                                                        String text,
                                                                        Entity<? extends EntityVersion> authoringSTAMP) {
        LOG.debug("Building FQN Description Semantic: " + text);
        RecordListBuilder<SemanticVersionRecord> versions = make();
        UUID descriptionSemanticUUID = uuidUtility.createUUID();
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
                                                                       Entity<? extends EntityVersion> authoringSTAMP) {
        LOG.debug("Building " + navigationPattern.description() + " Navigation Semantic");
        RecordListBuilder<SemanticVersionRecord> versions = make();
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

        LOG.debug("Building " + navigationPattern.description() + " Navigation Fields: " + "DES-" + destinations + " ORG-" + origins);
        MutableList<Object> navigationFields = Lists.mutable.empty();
        IntIdSet destinationIntIdSet;
        IntIdSet originIntIdSet;
        if (destinations == null) {
            destinationIntIdSet = IntIds.set.empty();
        } else {
            destinationIntIdSet = IntIds.set.of(destinations.stream().mapToInt(EntityProxy::nid).toArray());
        }
        if (origins == null) {
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
                                                                       Entity<? extends EntityVersion> authoringSTAMP) {
        LOG.debug("Building Identifier Semantic");
        RecordListBuilder<SemanticVersionRecord> versions = make();

        UUID navigationSemanticUUID = uuidUtility.createUUID();
        SemanticRecord semanticRecord = SemanticRecordBuilder.builder()
                .nid(EntityService.get().nidForUuids(navigationSemanticUUID))
                .leastSignificantBits(navigationSemanticUUID.getLeastSignificantBits())
                .mostSignificantBits(navigationSemanticUUID.getMostSignificantBits())
                .additionalUuidLongs(null)
                .patternNid(TinkarTerm.IDENTIFIER_PATTERN.nid())
                .referencedComponentNid(referencedComponentNid)
                .versions(versions.toImmutable())
                .build();

        LOG.debug("Building Identifier Semantic Fields");
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
                                                                    Entity<? extends EntityVersion> authoringSTAMP) {
        LOG.debug("Building Dialect Semantic");
        RecordListBuilder<SemanticVersionRecord> versions = make();
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

        LOG.debug("Building Dialect Semantic Fields");
        MutableList<Object> dialectFields = Lists.mutable.empty();
        dialectFields.add(dialect);

        versions.add(SemanticVersionRecordBuilder.builder()
                .chronology(semanticRecord)
                .stampNid(authoringSTAMP.nid())
                .fieldValues(dialectFields.toImmutable())
                .build());

        return SemanticRecordBuilder.builder(semanticRecord).versions(versions.toImmutable()).build();
    }

    protected Entity<? extends EntityVersion> createAxiomSyntaxSemantic(int referencedComponentNid,
                                                                        String axiomSyntax,
                                                                        Entity<? extends EntityVersion> authoringSTAMP) {
        LOG.debug("Building Axiom Syntax Semantic");
        RecordListBuilder<SemanticVersionRecord> versions = make();
        UUID axiomSyntaxSemantic = uuidUtility.createUUID();
        SemanticRecord semanticRecord = SemanticRecordBuilder.builder()
                .nid(EntityService.get().nidForUuids(axiomSyntaxSemantic))
                .leastSignificantBits(axiomSyntaxSemantic.getLeastSignificantBits())
                .mostSignificantBits(axiomSyntaxSemantic.getMostSignificantBits())
                .additionalUuidLongs(null)
                .patternNid(TinkarTerm.OWL_AXIOM_SYNTAX_PATTERN.nid())
                .referencedComponentNid(referencedComponentNid)
                .versions(versions.toImmutable())
                .build();

        LOG.debug("Building Axiom Syntax Semantic Fields");
        MutableList<Object> axiomSyntaxFields = Lists.mutable.empty();
        axiomSyntaxFields.add(axiomSyntax);

        versions.add(SemanticVersionRecordBuilder.builder()
                .chronology(semanticRecord)
                .stampNid(authoringSTAMP.nid())
                .fieldValues(axiomSyntaxFields.toImmutable())
                .build());

        return SemanticRecordBuilder.builder(semanticRecord).versions(versions.toImmutable()).build();
    }

    protected Entity<? extends EntityVersion> createCommentSemantic(int referencedComponentNid,
                                                                    String comment,
                                                                    Entity<? extends EntityVersion> authoringSTAMP) {
        LOG.debug("Building Comment Semantic");
        RecordListBuilder<SemanticVersionRecord> versions = make();
        UUID commentSemantic = uuidUtility.createUUID();
        SemanticRecord semanticRecord = SemanticRecordBuilder.builder()
                .nid(EntityService.get().nidForUuids(commentSemantic))
                .leastSignificantBits(commentSemantic.getLeastSignificantBits())
                .mostSignificantBits(commentSemantic.getMostSignificantBits())
                .additionalUuidLongs(null)
                .patternNid(TinkarTerm.COMMENT_PATTERN.nid())
                .referencedComponentNid(referencedComponentNid)
                .versions(versions.toImmutable())
                .build();

        LOG.debug("Building Comment Semantics Fields");
        MutableList<Object> commentFields = Lists.mutable.empty();
        commentFields.add(comment);

        versions.add(SemanticVersionRecordBuilder.builder()
                .chronology(semanticRecord)
                .stampNid(authoringSTAMP.nid())
                .fieldValues(commentFields.toImmutable())
                .build());

        return SemanticRecordBuilder.builder(semanticRecord).versions(versions.toImmutable()).build();
    }


    protected Entity<? extends EntityVersion> createSemanticFromPatternWithFields(int referencedComponentNid,
                                                                                  EntityProxy.Pattern pattern,
                                                                                  MutableList<Object> fields,
                                                                                  Entity<? extends EntityVersion> authoringSTAMP) {
        LOG.debug("Building Semantic Record");
        RecordListBuilder<SemanticVersionRecord> versions = make();
        UUID semanticUuid = uuidUtility.createUUID();
        SemanticRecord semanticRecord = SemanticRecordBuilder.builder()
                .nid(EntityService.get().nidForUuids(semanticUuid))
                .leastSignificantBits(semanticUuid.getLeastSignificantBits())
                .mostSignificantBits(semanticUuid.getMostSignificantBits())
                .additionalUuidLongs(null)
                .patternNid(pattern.nid())
                .referencedComponentNid(referencedComponentNid)
                .versions(versions.toImmutable())
                .build();

        versions.add(SemanticVersionRecordBuilder.builder()
                .chronology(semanticRecord)
                .stampNid(authoringSTAMP.nid())
                .fieldValues(fields.toImmutable())
                .build());
        return SemanticRecordBuilder.builder(semanticRecord).versions(versions.toImmutable()).build();
    }

    protected Entity<? extends EntityVersion> createStatedDefinitionSemantic(int referencedComponentNid,
                                                                             List<EntityProxy.Concept> originConceptList,
                                                                             Entity<? extends EntityVersion> authoringSTAMP) {
        LOG.debug("Building Stated Definition Semantic");
        RecordListBuilder<SemanticVersionRecord> versions = make();
        UUID statedDefinitionSemantic = uuidUtility.createUUID();
        SemanticRecord semanticRecord = SemanticRecordBuilder.builder()
                .nid(EntityService.get().nidForUuids(statedDefinitionSemantic))
                .leastSignificantBits(statedDefinitionSemantic.getLeastSignificantBits())
                .mostSignificantBits(statedDefinitionSemantic.getMostSignificantBits())
                .additionalUuidLongs(null)
                .patternNid(TinkarTerm.EL_PLUS_PLUS_STATED_AXIOMS_PATTERN.nid())
                .referencedComponentNid(referencedComponentNid)
                .versions(versions.toImmutable())
                .build();

        LOG.debug("Building Stated Definition Semantics Fields");
        MutableList<Object> statedDefinitionFields = Lists.mutable.empty();

        MutableList<EntityVertex> vertexMap = Lists.mutable.empty();
        MutableIntObjectMap<ImmutableIntList> successorMap = IntObjectMaps.mutable.empty();
        MutableIntIntMap predecessorMap = IntIntMaps.mutable.empty();

        int vertexIdx = 0;

        //Definition Root
        EntityVertex definitionRootVertex = createEntityVertex(vertexIdx++, TinkarTerm.DEFINITION_ROOT);
        vertexMap.add(definitionRootVertex);

        //Reference(s)
        MutableIntList referenceVertexIdxList = IntLists.mutable.empty();
        for (EntityProxy.Concept originConcept : originConceptList) {
            int referenceIndex = vertexIdx++;
            referenceVertexIdxList.add(referenceIndex);

            MutableIntObjectMap<Object> referenceProperty = IntObjectMaps.mutable.empty();
            referenceProperty.put(TinkarTerm.CONCEPT_REFERENCE.nid(), abstractObject(originConcept));

            EntityVertex referenceVertex = EntityVertex.make(originConcept);
            referenceVertex.setProperties(referenceProperty);
            referenceVertex.setVertexIndex(referenceIndex);

            referenceVertex.setMeaningNid(TinkarTerm.CONCEPT_REFERENCE.nid());
            vertexMap.add(referenceVertex);
        }

        //AND
        int andIndex = vertexIdx++;
        vertexMap.add(createEntityVertex(andIndex, TinkarTerm.AND));

        //Necessary Set
        int necessarySetIndex = vertexIdx++;
        vertexMap.add(createEntityVertex(necessarySetIndex, TinkarTerm.NECESSARY_SET));

        //Successor Map
        successorMap.put(0, IntLists.immutable.of(necessarySetIndex).toImmutable());
        successorMap.put(andIndex, referenceVertexIdxList.toImmutable());
        successorMap.put(necessarySetIndex, IntLists.immutable.of(andIndex).toImmutable());

        //Predecessor Map
        for (int referenceIdx : referenceVertexIdxList.toArray()) {
            predecessorMap.put(referenceIdx, andIndex);
        }
        predecessorMap.put(andIndex, necessarySetIndex);
        predecessorMap.put(necessarySetIndex, 0);

        statedDefinitionFields.add(new DiTreeEntity(definitionRootVertex, vertexMap.toImmutable(), successorMap.toImmutable(), predecessorMap.toImmutable()));

        versions.add(SemanticVersionRecordBuilder.builder()
                .chronology(semanticRecord)
                .stampNid(authoringSTAMP.nid())
                .fieldValues(statedDefinitionFields.toImmutable())
                .build());
        return SemanticRecordBuilder.builder(semanticRecord).versions(versions.toImmutable()).build();
    }


    public EntityVertex createEntityVertex(int vertexIndex, ConceptFacade conceptFacade) {
        MutableIntObjectMap<Object> properties = IntObjectMaps.mutable.empty();
        EntityVertex entityVertex = EntityVertex.make(conceptFacade);
        entityVertex.setVertexIndex(vertexIndex);
        entityVertex.setProperties(properties);
        return entityVertex;
    }
}
