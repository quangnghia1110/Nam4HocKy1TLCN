package studentConsulting.repository.common;

import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.JpaSpecificationExecutor;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;
import studentConsulting.model.entity.QuestionEntity;

import java.time.LocalDate;
import java.util.Date;
import java.util.List;

public interface StatisticsRepository extends JpaRepository<QuestionEntity, Integer>, JpaSpecificationExecutor<QuestionEntity> {

    @Query("SELECT COUNT(q) FROM QuestionEntity q WHERE q.user.id = :userId")
    Integer countTotalQuestionsByUser(@Param("userId") Integer userId);

    @Query("SELECT COUNT(q) FROM QuestionEntity q WHERE q.user.id = :userId AND q.views > 500")
    Integer countQuestionsWithMoreThan500Views(@Param("userId") Integer userId);

    @Query("SELECT COUNT(a) FROM ConsultationScheduleEntity a WHERE a.user.id = :userId")
    Integer countConsultationSchedulesByUser(@Param("userId") Integer userId);

    @Query("SELECT COUNT(r) FROM RatingEntity r WHERE r.user.id = :userId")
    Integer countRatingsByUser(@Param("userId") Integer userId);

    @Query("SELECT COUNT(q) FROM QuestionEntity q " +
            "WHERE q.department.id IN :departmentIds " +
            "AND (:startDate IS NULL OR q.createdAt >= :startDate) " +
            "AND (:endDate IS NULL OR q.createdAt <= :endDate)")
    Integer countQuestionsByDepartmentIds(
            @Param("departmentIds") List<Integer> departmentIds,
            @Param("startDate") LocalDate startDate,
            @Param("endDate") LocalDate endDate
    );

    @Query("SELECT COUNT(DISTINCT q.question.id) " +
            "FROM ForwardQuestionEntity q " +
            "WHERE (q.fromDepartment.id IN :departmentId OR q.toDepartment.id IN :departmentId " +
            "OR q.createdBy.id = :consultantId) " +
            "AND q.statusForward = true " +
            "AND (:startDate IS NULL OR q.createdAt >= :startDate) " +
            "AND (:endDate IS NULL OR q.createdAt <= :endDate)")
    Integer countDistinctQuestionsForwarded(
            @Param("departmentId") List<Integer> departmentId,
            @Param("consultantId") Integer consultantId,
            @Param("startDate") LocalDate startDate,
            @Param("endDate") LocalDate endDate);


    @Query("SELECT COUNT(q) FROM QuestionEntity q " +
            "WHERE q.statusDelete = true " +
            "AND (q.department.id IN :departmentIds OR EXISTS (" +
            "    SELECT 1 FROM DeletionLogEntity dl " +
            "    JOIN AccountEntity a ON a.email = dl.deletedBy " +
            "    JOIN UserInformationEntity u ON u.account.id = a.id " +
            "    WHERE dl.question.id = q.id AND u.id = :consultantId)) " +
            "AND (:startDate IS NULL OR q.createdAt >= :startDate) " +
            "AND (:endDate IS NULL OR q.createdAt <= :endDate)")
    Integer countDeletedQuestions(
            @Param("departmentIds") List<Integer> departmentIds,
            @Param("consultantId") Integer consultantId,
            @Param("startDate") LocalDate startDate,
            @Param("endDate") LocalDate endDate
    );

    @Query("SELECT COUNT(q) FROM QuestionEntity q JOIN q.answers a " +
            "WHERE (q.department.id IN :departmentIds OR a.user.id = :consultantId) " +
            "AND (:startDate IS NULL OR q.createdAt >= :startDate) " +
            "AND (:endDate IS NULL OR q.createdAt <= :endDate)")
    Integer countAnsweredQuestions(
            @Param("departmentIds") List<Integer> departmentIds,
            @Param("consultantId") Integer consultantId,
            @Param("startDate") LocalDate startDate,
            @Param("endDate") LocalDate endDate
    );

    @Query("SELECT COUNT(a) FROM AnswerEntity a " +
            "WHERE (a.question.department.id IN :departmentIds OR a.user.id = :consultantId) " +
            "AND a.statusApproval = true " +
            "AND (:startDate IS NULL OR a.createdAt >= :startDate) " +
            "AND (:endDate IS NULL OR a.createdAt <= :endDate)")
    Integer countApprovedAnswers(
            @Param("departmentIds") List<Integer> departmentIds,
            @Param("consultantId") Integer consultantId,
            @Param("startDate") LocalDate startDate,
            @Param("endDate") LocalDate endDate
    );

    @Query("SELECT COUNT(c) FROM ConsultationScheduleEntity c " +
            "WHERE (c.department.id IN :departmentIds OR c.consultant.id = :consultantId) " +
            "AND c.statusConfirmed = true " +
            "AND (:startDate IS NULL OR c.createdAt >= :startDate) " +
            "AND (:endDate IS NULL OR c.createdAt <= :endDate)")
    Integer countConfirmedSchedules(
            @Param("departmentIds") List<Integer> departmentIds,
            @Param("consultantId") Integer consultantId,
            @Param("startDate") LocalDate startDate,
            @Param("endDate") LocalDate endDate
    );

    @Query("SELECT COUNT(c) FROM ConversationEntity c " +
            "WHERE (c.department.id IN :departmentIds OR c.consultant.id = :consultantId) " +
            "AND (:startDate IS NULL OR c.createdAt >= :startDate) " +
            "AND (:endDate IS NULL OR c.createdAt <= :endDate)")
    Integer countConversations(
            @Param("departmentIds") List<Integer> departmentIds,
            @Param("consultantId") Integer consultantId,
            @Param("startDate") LocalDate startDate,
            @Param("endDate") LocalDate endDate
    );

    @Query("SELECT COUNT(r) FROM RatingEntity r " +
            "WHERE r.department.id IN :departmentIds " +
            "AND (:startDate IS NULL OR r.submittedAt >= :startDate) " +
            "AND (:endDate IS NULL OR r.submittedAt <= :endDate)")
    Integer countRatingsByDepartmentIds(
            @Param("departmentIds") List<Integer> departmentIds,
            @Param("startDate") LocalDate startDate,
            @Param("endDate") LocalDate endDate
    );

    @Query("SELECT COUNT(q) FROM CommonQuestionEntity q " +
            "WHERE q.department.id IN :departmentIds " +
            "AND (:startDate IS NULL OR q.createdAt >= :startDate) " +
            "AND (:endDate IS NULL OR q.createdAt <= :endDate)")
    Integer countCommonQuestionsByDepartmentIds(
            @Param("departmentIds") List<Integer> departmentIds,
            @Param("startDate") LocalDate startDate,
            @Param("endDate") LocalDate endDate
    );

    @Query("SELECT COUNT(p) FROM PostEntity p " +
            "WHERE (p.user.id = :consultantId) " +
            "AND p.isApproved = true " +
            "AND (:startDate IS NULL OR p.createdAt >= :startDate) " +
            "AND (:endDate IS NULL OR p.createdAt <= :endDate)")
    Integer countApprovedPosts(
            @Param("consultantId") Integer consultantId,
            @Param("startDate") LocalDate startDate,
            @Param("endDate") LocalDate endDate
    );

    @Query("SELECT COUNT(r) FROM RatingEntity r " +
            "WHERE r.consultant.id = :consultantId " +
            "AND (:startDate IS NULL OR r.submittedAt >= :startDate) " +
            "AND (:endDate IS NULL OR r.submittedAt <= :endDate)")
    Integer countRatingsByConsultantId(
            @Param("consultantId") Integer consultantId,
            @Param("startDate") LocalDate startDate,
            @Param("endDate") LocalDate endDate
    );

    @Query("SELECT COUNT(a) FROM AccountEntity a " +
            "WHERE a.department.id IN :departmentIds " +
            "AND a.role.name = 'ROLE_TUVANVIEN' " +
            "AND (:startDate IS NULL OR a.createdAt >= :startDate) " +
            "AND (:endDate IS NULL OR a.createdAt <= :endDate)")
    Integer countConsultantsByDepartmentIds(
            @Param("departmentIds") List<Integer> departmentIds,
            @Param("startDate") LocalDate startDate,
            @Param("endDate") LocalDate endDate
    );

    @Query("SELECT COUNT(DISTINCT m.receiver.id) FROM MessageEntity m " +
            "JOIN m.sender s JOIN s.account a JOIN a.department d " +
            "WHERE d.id IN :departmentIds " +
            "AND (:startDate IS NULL OR FUNCTION('DATE', m.date) >= :startDate) " +
            "AND (:endDate IS NULL OR FUNCTION('DATE', m.date) <= :endDate)")
    Integer countUniqueUsersAdvisedByDepartmentIds(
            @Param("departmentIds") List<Integer> departmentIds,
            @Param("startDate") Date startDate,
            @Param("endDate") Date endDate
    );

    @Query("SELECT COUNT(DISTINCT m.receiver.id) FROM MessageEntity m " +
            "WHERE m.sender.id = :consultantId " +
            "AND (:startDate IS NULL OR FUNCTION('DATE', m.date) >= :startDate) " +
            "AND (:endDate IS NULL OR FUNCTION('DATE', m.date) <= :endDate)")
    Integer countUniqueUsersAdvisedByConsultant(
            @Param("consultantId") Integer consultantId,
            @Param("startDate") Date startDate,
            @Param("endDate") Date endDate
    );

}

