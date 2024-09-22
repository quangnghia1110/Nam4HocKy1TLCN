package studentConsulting.repository;

import java.time.LocalDate;

import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.JpaSpecificationExecutor;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;

import studentConsulting.model.entity.questionAnswer.QuestionEntity;

public interface StatisticsRepository extends JpaRepository<QuestionEntity, Integer>, JpaSpecificationExecutor<QuestionEntity> {

    @Query("SELECT COUNT(q) FROM QuestionEntity q WHERE q.user.id = :userId")
    Integer countTotalQuestionsByUser(@Param("userId") Integer userId);

    @Query("SELECT COUNT(q) FROM QuestionEntity q WHERE q.user.id = :userId AND q.views > 500")
    Integer countQuestionsWithMoreThan500Views(@Param("userId") Integer userId);

    @Query("SELECT COUNT(a) FROM ConsultationScheduleEntity a WHERE a.user.id = :userId")
    Integer countConsultationSchedulesByUser(@Param("userId") Integer userId);

    @Query("SELECT COUNT(r) FROM RatingEntity r WHERE r.user.id = :userId")
    Integer countRatingsByUser(@Param("userId") Integer userId);






    @Query("SELECT COUNT(q) FROM QuestionEntity q WHERE q.department.id = :departmentId AND q.createdAt = :date")
    Integer countQuestionsByDepartmentIdAndDate(
        @Param("departmentId") Integer departmentId, 
        @Param("date") LocalDate date
    );

    @Query("SELECT COUNT(DISTINCT q.toDepartment.id) FROM ForwardQuestionEntity q JOIN q.fromDepartment.accounts a WHERE a.id = :consultantId AND q.statusForward = true")
    Integer countDistinctToDepartmentsByConsultantIdAndStatusForwardedTrue(@Param("consultantId") Integer consultantId);    
    
    @Query("SELECT COUNT(q) FROM QuestionEntity q JOIN q.answers a WHERE a.user.id = :consultantId AND q.statusDelete = true")
    Integer countByConsultantIdAndDeletedTrue(@Param("consultantId") Integer consultantId);

    @Query("SELECT COUNT(q) FROM QuestionEntity q JOIN q.answers a WHERE a.user.id = :consultantId")
    Integer countByConsultantIdAndAnsweredTrue(@Param("consultantId") Integer consultantId);

    @Query("SELECT COUNT(a) FROM AnswerEntity a WHERE a.user.id = :consultantId AND a.statusApproval = true")
    Integer countByConsultantIdAndStatusApprovalTrue(@Param("consultantId") Integer consultantId);
    
    @Query("SELECT COUNT(c) FROM ConsultationScheduleEntity c WHERE c.consultant.id = :consultantId AND c.statusConfirmed = true")
    Integer countByConsultantIdAndStatusConfirmedTrue(@Param("consultantId") Integer consultantId);

    @Query("SELECT COUNT(p) FROM PostEntity p WHERE p.user.id = :consultantId AND p.isApproved = true")
    Integer countByConsultantIdAndPublishedTrue(@Param("consultantId") Integer consultantId);

    @Query("SELECT COUNT(c) FROM ConversationEntity c WHERE c.consultant.id = :consultantId")
    Integer countByConsultantId(@Param("consultantId") Integer consultantId);
}
