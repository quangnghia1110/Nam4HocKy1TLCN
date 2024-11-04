package studentConsulting.repository.actor;

import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.repository.JpaSpecificationExecutor;
import org.springframework.data.jpa.repository.Modifying;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.PagingAndSortingRepository;
import org.springframework.data.repository.query.Param;
import org.springframework.stereotype.Repository;
import studentConsulting.model.entity.QuestionEntity;

import java.util.List;
import java.util.Optional;

@Repository
public interface QuestionRepository extends PagingAndSortingRepository<QuestionEntity, Integer>, JpaSpecificationExecutor<QuestionEntity> {
    Page<QuestionEntity> findByDepartmentId(Integer departmentId, Pageable pageable);

    // Tìm tất cả câu hỏi theo userId
    @Query("SELECT q FROM QuestionEntity q WHERE q.user.id = :userId")
    Page<QuestionEntity> findByUserId(@Param("userId") Integer userId, Pageable pageable);

    // Tìm câu hỏi theo tiêu đề và phòng ban
    @Query("SELECT q FROM QuestionEntity q WHERE q.user.id = :userId AND q.title LIKE %:title% AND q.department.id = :departmentId")
    Page<QuestionEntity> searchQuestionsByTitleAndDepartment(@Param("userId") Integer userId, @Param("title") String title, @Param("departmentId") Integer departmentId, Pageable pageable);

    // Lọc câu hỏi theo phòng ban
    @Query("SELECT q FROM QuestionEntity q WHERE q.user.id = :userId AND q.department.id = :departmentId")
    Page<QuestionEntity> filterMyQuestionsByDepartment(@Param("userId") Integer userId, @Param("departmentId") Integer departmentId, Pageable pageable);

    @Modifying
    @Query("UPDATE QuestionEntity q SET q.statusDelete = true WHERE q.id = :questionId")
    void softDeleteQuestion(@Param("questionId") Integer questionId);

    @Query("SELECT q FROM QuestionEntity q WHERE q.department.id = :departmentId AND q.statusApproval = true")
    List<QuestionEntity> findApprovedQuestionsByDepartment(@Param("departmentId") Integer departmentId);

    @Query("SELECT q FROM QuestionEntity q WHERE q.id = :questionId AND q.department.id = :departmentId")
    Optional<QuestionEntity> findByIdAndDepartmentId(@Param("questionId") Integer questionId, @Param("departmentId") Integer departmentId);

    @Query("SELECT q FROM QuestionEntity q WHERE q.parentQuestion.id = :parentQuestionId")
    List<QuestionEntity> findFollowUpQuestionsByParentId(@Param("parentQuestionId") Integer parentQuestionId);

}

