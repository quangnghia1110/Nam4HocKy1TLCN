package studentConsulting.repository.question_answer;

import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.repository.JpaSpecificationExecutor;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.PagingAndSortingRepository;
import org.springframework.data.repository.query.Param;
import org.springframework.stereotype.Repository;
import studentConsulting.model.entity.question_answer.AnswerEntity;

import java.util.List;
import java.util.Optional;

@Repository
public interface AnswerRepository extends PagingAndSortingRepository<AnswerEntity, Integer>, JpaSpecificationExecutor<AnswerEntity> {

    @Query("SELECT a FROM AnswerEntity a WHERE a.question.id = :questionId ORDER BY a.createdAt ASC")
    Optional<AnswerEntity> findFirstAnswerByQuestionId(@Param("questionId") Integer questionId);

    @Query("SELECT a FROM AnswerEntity a WHERE a.question.id = :questionId")
    List<AnswerEntity> findAnswersByQuestionId(@Param("questionId") Integer questionId);

    boolean existsByQuestionId(Integer questionId);

    @Query("SELECT a FROM AnswerEntity a WHERE a.statusApproval = true AND a.statusAnswer = false")
    Page<AnswerEntity> findAnswersForReview(@Param("title") String title, Pageable pageable);

    @Query("SELECT a FROM AnswerEntity a WHERE a.question.department.id = :departmentId AND a.statusApproval = true")
    List<AnswerEntity> findApprovedAnswersByDepartment(@Param("departmentId") Integer departmentId);

    @Query("SELECT a FROM AnswerEntity a WHERE a.id = :id AND a.question.department.id = :departmentId")
    Optional<AnswerEntity> findByIdAndDepartmentId(@Param("id") Integer id, @Param("departmentId") Integer departmentId);

    @Query("SELECT COUNT(a) > 0 FROM AnswerEntity a WHERE a.question.user.id = :userId AND a.user.id = :consultantId")
    boolean hasConsultantAnsweredUserQuestions(Integer userId, Integer consultantId);

    @Query("SELECT a FROM AnswerEntity a WHERE a.id = :answerId AND a.user.account.email = :email")
    Optional<AnswerEntity> findByIdAndUserAccountEmail(@Param("answerId") Integer answerId, @Param("email") String email);

    @Query("SELECT CASE WHEN COUNT(q) > 0 THEN true ELSE false END " +
            "FROM QuestionEntity q " +
            "WHERE q.department.id = :departmentId AND q.id = :questionId")
    boolean existsByDepartmentIdAndQuestionId(@Param("departmentId") Integer departmentId,
                                              @Param("questionId") Integer questionId);

}
