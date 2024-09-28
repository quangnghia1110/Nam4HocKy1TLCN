package studentConsulting.repository;

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
}
