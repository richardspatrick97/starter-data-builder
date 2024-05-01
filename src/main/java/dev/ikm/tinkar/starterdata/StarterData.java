package dev.ikm.tinkar.starterdata;

import dev.ikm.tinkar.common.service.CachingService;
import dev.ikm.tinkar.common.service.PrimitiveData;
import dev.ikm.tinkar.common.service.ServiceKeys;
import dev.ikm.tinkar.common.service.ServiceProperties;
import dev.ikm.tinkar.common.util.time.DateTimeUtil;
import dev.ikm.tinkar.entity.*;
import dev.ikm.tinkar.terms.EntityProxy;
import dev.ikm.tinkar.terms.TinkarTerm;
import org.eclipse.collections.api.factory.Lists;
import org.eclipse.collections.api.list.ImmutableList;
import org.eclipse.collections.api.list.MutableList;

import java.io.File;
import java.util.ArrayList;
import java.util.List;
import java.util.function.Function;
import java.util.logging.Logger;

public class StarterData {

    private static final Logger LOG = Logger.getLogger(StarterData.class.getSimpleName());

    private final File datastore;
    private Entity<? extends EntityVersion> authoringSTAMP;
    private final List<Entity<? extends EntityVersion>> starterDataEntities;

    private final UUIDUtility uuidUtility;
    private final STAMPUtility stampUtility;
    private final ConceptUtility conceptUtility;
    private final PatternUtility patternUtility;
    private final SemanticUtility semanticUtility;

    public StarterData(File datastore, UUIDUtility uuidUtility) {
        this.datastore = datastore;
        this.starterDataEntities = new ArrayList<>();

        this.uuidUtility = uuidUtility;
        this.stampUtility = new STAMPUtility(this.uuidUtility);
        this.conceptUtility = new ConceptUtility(this.uuidUtility);
        this.patternUtility = new PatternUtility(this.uuidUtility);
        this.semanticUtility = new SemanticUtility(this.uuidUtility);
    }

    public StarterData init(){
        LOG.info("Starting database");
        LOG.info("Loading data from " + datastore.getAbsolutePath());
        CachingService.clearAll();
        ServiceProperties.set(ServiceKeys.DATA_STORE_ROOT, datastore);
        PrimitiveData.selectControllerByName("Open SpinedArrayStore");
        PrimitiveData.start();
        return this;
    }

    public Entity<? extends EntityVersion> getAuthoringSTAMP(){
        return authoringSTAMP;
    }

    public StarterData build() {
        starterDataEntities.forEach(entity -> EntityService.get().putEntity(entity));

        LOG.info("STAMPs: " + starterDataEntities.stream().filter(entity -> entity instanceof StampRecord).count());
        LOG.info("Concepts: " + starterDataEntities.stream().filter(entity -> entity instanceof ConceptRecord).count());
        LOG.info("Semantics: " + starterDataEntities.stream().filter(entity -> entity instanceof SemanticRecord).count());
        LOG.info("Patterns: " + starterDataEntities.stream().filter(entity -> entity instanceof PatternRecord).count());

        starterDataEntities.clear();
        return this;
    }

    public StarterData shutdown(){
        PrimitiveData.stop();
        return this;
    }

    public StarterData authoringSTAMP(EntityProxy.Concept status, long time, EntityProxy.Concept author, EntityProxy.Concept module, EntityProxy.Concept path){
        authoringSTAMP = stampUtility.createSTAMP(status, time, author, module, path);
        starterDataEntities.add(authoringSTAMP);
        return this;
    }

    public ConceptBuilder concept(EntityProxy.Concept concept){
        return new ConceptBuilder(concept);
    }

    public class ConceptBuilder{

        private final List<Entity<? extends EntityVersion>> builderEntities;
        private final int conceptNid;
        private final EntityProxy.Concept conceptProxy;

        protected ConceptBuilder(EntityProxy.Concept concept) {
            this.builderEntities = new ArrayList<>();
            this.conceptNid = concept.nid();
            this.conceptProxy = concept;
            this.builderEntities.add(conceptUtility.createConcept(concept, authoringSTAMP));
        }

        public ConceptBuilder fullyQualifiedName(String text, EntityProxy.Concept dialectAcceptability){
            var fqnSemantic = semanticUtility.createDescriptionSemantic(conceptNid, TinkarTerm.FULLY_QUALIFIED_NAME_DESCRIPTION_TYPE, text, authoringSTAMP);
            builderEntities.add(fqnSemantic);
            builderEntities.add(semanticUtility.createDialectSemantic(fqnSemantic.nid(), dialectAcceptability, authoringSTAMP));
            return this;
        }

        public ConceptBuilder synonym(String text, EntityProxy.Concept dialectAcceptability){
            var synonymSemantic = semanticUtility.createDescriptionSemantic(conceptNid, TinkarTerm.REGULAR_NAME_DESCRIPTION_TYPE, text, authoringSTAMP);
            builderEntities.add(synonymSemantic);
            builderEntities.add(semanticUtility.createDialectSemantic(synonymSemantic.nid(), dialectAcceptability, authoringSTAMP));
            return this;
        }

        public ConceptBuilder definition(String text, EntityProxy.Concept dialectAcceptability){
            var definitionSemantic = semanticUtility.createDescriptionSemantic(conceptNid, TinkarTerm.DEFINITION_DESCRIPTION_TYPE, text, authoringSTAMP);
            builderEntities.add(definitionSemantic);
            builderEntities.add(semanticUtility.createDialectSemantic(definitionSemantic.nid(), dialectAcceptability, authoringSTAMP));
            return this;
        }

        public ConceptBuilder statedNavigation(List<EntityProxy.Concept> destinations, List<EntityProxy.Concept> origins){
            builderEntities.add(semanticUtility.createNavigationSemantic(conceptNid, TinkarTerm.STATED_NAVIGATION_PATTERN, destinations, origins, authoringSTAMP));
            return this;
        }

        public ConceptBuilder inferredNavigation(List<EntityProxy.Concept> destinations, List<EntityProxy.Concept> origins){
            builderEntities.add(semanticUtility.createNavigationSemantic(conceptNid, TinkarTerm.INFERRED_NAVIGATION_PATTERN, destinations, origins, authoringSTAMP));
            return this;
        }

        public ConceptBuilder identifier(EntityProxy.Concept source, String id){
            builderEntities.add(semanticUtility.createIdentifierSemantic(conceptNid, source, id, authoringSTAMP));
            return this;
        }

        public ConceptBuilder axiomSyntax(String axiomSyntax){
            builderEntities.add(semanticUtility.createAxiomSyntaxSemantic(conceptNid, axiomSyntax, authoringSTAMP));
            return this;
        }

        public ConceptBuilder comment(String comment){
            builderEntities.add(semanticUtility.createCommentSemantic(conceptNid, comment, authoringSTAMP));
            return this;
        }

        public ConceptBuilder pathMembership(){
            builderEntities.add(
                    semanticUtility.createSemanticFromPatternWithFields(conceptNid, TinkarTerm.PATHS_PATTERN, Lists.mutable.empty(), authoringSTAMP)
            );
            return this;
        }

        public ConceptBuilder pathOrigin(EntityProxy.Concept originPath){
            MutableList<Object> fields = Lists.mutable.of(originPath, DateTimeUtil.epochMsToInstant(Long.MAX_VALUE));
            builderEntities.add(
                    semanticUtility.createSemanticFromPatternWithFields(conceptNid, TinkarTerm.PATH_ORIGINS_PATTERN, fields, authoringSTAMP)
            );
            return this;
        }

        public ConceptBuilder tinkarBaseModelMembership(){
            builderEntities.add(
                    semanticUtility.createSemanticFromPatternWithFields(conceptNid, TinkarTerm.TINKAR_BASE_MODEL_COMPONENT_PATTERN, Lists.mutable.empty(), authoringSTAMP)
            );
            return this;
        }

        public ConceptBuilder kometBaseModelMembership(){
            starterDataEntities.add(
                    semanticUtility.createSemanticFromPatternWithFields(conceptNid, TinkarTerm.KOMET_BASE_MODEL_COMPONENT_PATTERN, Lists.mutable.empty(), authoringSTAMP)
            );
            return this;
        }

        public ConceptBuilder statedDefinition(List<EntityProxy.Concept> originConceptList){
            builderEntities.add(semanticUtility.createStatedDefinitionSemantic(conceptNid, originConceptList, authoringSTAMP));
            return this;
        }

        public ConceptBuilder authoringMembership(){
            return this;
        }

        public ConceptBuilder coreMembership(){
            return this;
        }

        public ConceptBuilder kometMembership(){
            return this;
        }


        public EntityProxy.Concept build(){
            starterDataEntities.addAll(builderEntities);
            return conceptProxy;
        }
    }

    public PatternBuilder pattern(EntityProxy.Pattern pattern){
        return new PatternBuilder(pattern);
    }

    public class PatternBuilder{

        private final int patternNid;
        private final EntityProxy.Pattern patternProxy;
        private EntityProxy.Concept meaning;
        private EntityProxy.Concept purpose;
        private Function<Integer, ImmutableList<FieldDefinitionRecord>> fieldDefinitionFunction;
        private final MutableList<FieldDefinitionRecord> fieldDefinitions;

        protected PatternBuilder(EntityProxy.Pattern pattern) {
            this.patternNid = pattern.nid();
            this.patternProxy = pattern;
            this.fieldDefinitions = Lists.mutable.empty();
        }

        public PatternBuilder meaning(EntityProxy.Concept meaning){
            this.meaning = meaning;
            return this;
        }

        public PatternBuilder purpose(EntityProxy.Concept purpose){
            this.purpose = purpose;
            return this;
        }

        public PatternBuilder fieldDefinition(EntityProxy.Concept meaning, EntityProxy.Concept purpose, EntityProxy.Concept dataType){
            fieldDefinitions.add(FieldDefinitionRecordBuilder.builder()
                    .patternNid(patternNid)
                    .meaningNid(meaning.nid())
                    .purposeNid(purpose.nid())
                    .dataTypeNid(dataType.nid())
                    .indexInPattern(fieldDefinitions.size())
                    .patternVersionStampNid(authoringSTAMP.nid())
                    .build());
            return this;
        }

        public PatternBuilder fullyQualifiedName(String text, EntityProxy.Concept dialectAcceptability){
            var fqnSemantic = semanticUtility.createDescriptionSemantic(patternNid, TinkarTerm.FULLY_QUALIFIED_NAME_DESCRIPTION_TYPE, text, authoringSTAMP);
            starterDataEntities.add(fqnSemantic);
            starterDataEntities.add(semanticUtility.createDialectSemantic(fqnSemantic.nid(), dialectAcceptability, authoringSTAMP));
            return this;
        }

        public PatternBuilder synonym(String text, EntityProxy.Concept dialectAcceptability){
            var synonymSemantic = semanticUtility.createDescriptionSemantic(patternNid, TinkarTerm.REGULAR_NAME_DESCRIPTION_TYPE, text, authoringSTAMP);
            starterDataEntities.add(synonymSemantic);
            starterDataEntities.add(semanticUtility.createDialectSemantic(synonymSemantic.nid(), dialectAcceptability, authoringSTAMP));
            return this;
        }

        public PatternBuilder definition(String text, EntityProxy.Concept dialectAcceptability){
            var definitionSemantic = semanticUtility.createDescriptionSemantic(patternNid, TinkarTerm.DEFINITION_DESCRIPTION_TYPE, text, authoringSTAMP);
            starterDataEntities.add(definitionSemantic);
            starterDataEntities.add(semanticUtility.createDialectSemantic(definitionSemantic.nid(), dialectAcceptability, authoringSTAMP));
            return this;
        }

        public PatternBuilder tinkarBaseModelMembership(){
            starterDataEntities.add(
                    semanticUtility.createSemanticFromPatternWithFields(patternNid, TinkarTerm.TINKAR_BASE_MODEL_COMPONENT_PATTERN, Lists.mutable.empty(), authoringSTAMP)
            );
            return this;
        }

        public PatternBuilder kometBaseModelMembership(){
            starterDataEntities.add(
                    semanticUtility.createSemanticFromPatternWithFields(patternNid, TinkarTerm.KOMET_BASE_MODEL_COMPONENT_PATTERN, Lists.mutable.empty(), authoringSTAMP)
            );
            return this;
        }

        public EntityProxy.Pattern build(){
            assert patternProxy.description() != null;
            starterDataEntities.add(patternUtility.createPattern(
                    patternProxy,
                    meaning,
                    purpose,
                    authoringSTAMP,
                    fieldDefinitions.toImmutable()));
            Entity<? extends EntityVersion> descriptionSemantic = semanticUtility.createDescriptionSemantic(
                    patternNid,
                    TinkarTerm.REGULAR_NAME_DESCRIPTION_TYPE,
                    patternProxy.description(),
                    authoringSTAMP);
            starterDataEntities.add(descriptionSemantic);
            starterDataEntities.add(semanticUtility.createDialectSemantic(descriptionSemantic.nid(), TinkarTerm.PREFERRED, authoringSTAMP));


            return patternProxy;
        }
    }
}
