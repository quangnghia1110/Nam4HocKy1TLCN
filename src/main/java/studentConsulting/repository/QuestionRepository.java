package studentConsulting.repository;

import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;
import org.springframework.stereotype.Repository;

import studentConsulting.model.entity.questionAnswer.QuestionEntity;

@Repository
public interface QuestionRepository extends JpaRepository<QuestionEntity, Integer> {
    Page<QuestionEntity> findByDepartmentId(Integer departmentId, Pageable pageable);

 // Lọc câu hỏi đã trả lời theo tiêu đề và phòng ban
    @Query("SELECT q FROM QuestionEntity q WHERE q.user.id = :userId AND q.department.id = :departmentId AND q.title LIKE %:title% AND EXISTS (SELECT a FROM AnswerEntity a WHERE a.question.id = q.id)")
    Page<QuestionEntity> findAnsweredQuestionsByTitleAndDepartment(@Param("userId") Integer userId, @Param("title") String title, @Param("departmentId") Integer departmentId, Pageable pageable);

    // Lọc câu hỏi chưa trả lời theo tiêu đề và phòng ban
    @Query("SELECT q FROM QuestionEntity q WHERE q.user.id = :userId AND q.department.id = :departmentId AND q.title LIKE %:title% AND NOT EXISTS (SELECT a FROM AnswerEntity a WHERE a.question.id = q.id)")
    Page<QuestionEntity> findNotAnsweredQuestionsByTitleAndDepartment(@Param("userId") Integer userId, @Param("title") String title, @Param("departmentId") Integer departmentId, Pageable pageable);

    // Lọc câu hỏi công khai theo tiêu đề và phòng ban
    @Query("SELECT q FROM QuestionEntity q WHERE q.user.id = :userId AND q.department.id = :departmentId AND q.statusPublic = :isPublic AND q.title LIKE %:title%")
    Page<QuestionEntity> findByUserIdAndStatusPublicTitleAndDepartment(@Param("userId") Integer userId, @Param("isPublic") boolean isPublic, @Param("title") String title, @Param("departmentId") Integer departmentId, Pageable pageable);

    // Lọc câu hỏi bị xóa theo tiêu đề và phòng ban
    @Query("SELECT q FROM QuestionEntity q WHERE q.user.id = :userId AND q.department.id = :departmentId AND q.statusDelete = :isDeleted AND q.title LIKE %:title%")
    Page<QuestionEntity> findByUserIdAndStatusDeleteTitleAndDepartment(@Param("userId") Integer userId, @Param("isDeleted") boolean isDeleted, @Param("title") String title, @Param("departmentId") Integer departmentId, Pageable pageable);

    // Lọc câu hỏi đã duyệt theo tiêu đề và phòng ban
    @Query("SELECT q FROM QuestionEntity q WHERE q.user.id = :userId AND q.department.id = :departmentId AND q.statusApproval = :isApproved AND q.title LIKE %:title%")
    Page<QuestionEntity> findByUserIdAndStatusApprovalTitleAndDepartment(@Param("userId") Integer userId, @Param("isApproved") boolean isApproved, @Param("title") String title, @Param("departmentId") Integer departmentId, Pageable pageable);

    // Tìm kiếm câu hỏi theo tiêu đề
    @Query("SELECT q FROM QuestionEntity q WHERE q.user.id = :userId AND q.title LIKE %:title%")
    Page<QuestionEntity> searchQuestionsByTitle(@Param("userId") Integer userId, @Param("title") String title, Pageable pageable);

    // Lọc câu hỏi chưa trả lời theo tiêu đề
    @Query("SELECT q FROM QuestionEntity q WHERE q.user.id = :userId AND q.title LIKE %:title% AND NOT EXISTS (SELECT a FROM AnswerEntity a WHERE a.question.id = q.id)")
    Page<QuestionEntity> findNotAnsweredQuestionsByTitle(@Param("userId") Integer userId, @Param("title") String title, Pageable pageable);

    // Lọc câu hỏi đã trả lời theo tiêu đề
    @Query("SELECT q FROM QuestionEntity q WHERE q.user.id = :userId AND q.title LIKE %:title% AND EXISTS (SELECT a FROM AnswerEntity a WHERE a.question.id = q.id)")
    Page<QuestionEntity> findAnsweredQuestionsByTitle(@Param("userId") Integer userId, @Param("title") String title, Pageable pageable);

    // Lọc câu hỏi công khai theo tiêu đề
    @Query("SELECT q FROM QuestionEntity q WHERE q.user.id = :userId AND q.statusPublic = :isPublic AND q.title LIKE %:title%")
    Page<QuestionEntity> findByUserIdAndStatusPublicTitle(@Param("userId") Integer userId, @Param("isPublic") boolean isPublic, @Param("title") String title, Pageable pageable);

    // Lọc câu hỏi bị xóa theo tiêu đề
    @Query("SELECT q FROM QuestionEntity q WHERE q.user.id = :userId AND q.statusDelete = :isDeleted AND q.title LIKE %:title%")
    Page<QuestionEntity> findByUserIdAndStatusDeleteTitle(@Param("userId") Integer userId, @Param("isDeleted") boolean isDeleted, @Param("title") String title, Pageable pageable);

    // Lọc câu hỏi đã duyệt theo tiêu đề
    @Query("SELECT q FROM QuestionEntity q WHERE q.user.id = :userId AND q.statusApproval = :isApproved AND q.title LIKE %:title%")
    Page<QuestionEntity> findByUserIdAndStatusApprovalTitle(@Param("userId") Integer userId, @Param("isApproved") boolean isApproved, @Param("title") String title, Pageable pageable);

    // Lọc câu hỏi đã trả lời theo phòng ban
    @Query("SELECT q FROM QuestionEntity q WHERE q.user.id = :userId AND q.department.id = :departmentId AND EXISTS (SELECT a FROM AnswerEntity a WHERE a.question.id = q.id)")
    Page<QuestionEntity> findAnsweredQuestionsByDepartment(@Param("userId") Integer userId, @Param("departmentId") Integer departmentId, Pageable pageable);

    // Lọc câu hỏi chưa trả lời theo phòng ban
    @Query("SELECT q FROM QuestionEntity q WHERE q.user.id = :userId AND q.department.id = :departmentId AND NOT EXISTS (SELECT a FROM AnswerEntity a WHERE a.question.id = q.id)")
    Page<QuestionEntity> findNotAnsweredQuestionsByDepartment(@Param("userId") Integer userId, @Param("departmentId") Integer departmentId, Pageable pageable);

    // Lọc câu hỏi công khai theo phòng ban
    @Query("SELECT q FROM QuestionEntity q WHERE q.user.id = :userId AND q.statusPublic = :isPublic AND q.department.id = :departmentId")
    Page<QuestionEntity> findByUserIdAndStatusPublicAndDepartment(@Param("userId") Integer userId, @Param("isPublic") boolean isPublic, @Param("departmentId") Integer departmentId, Pageable pageable);

    // Lọc câu hỏi bị xóa theo phòng ban
    @Query("SELECT q FROM QuestionEntity q WHERE q.user.id = :userId AND q.statusDelete = :isDeleted AND q.department.id = :departmentId")
    Page<QuestionEntity> findByUserIdAndStatusDeleteAndDepartment(@Param("userId") Integer userId, @Param("isDeleted") boolean isDeleted, @Param("departmentId") Integer departmentId, Pageable pageable);

    // Lọc câu hỏi đã duyệt theo phòng ban
    @Query("SELECT q FROM QuestionEntity q WHERE q.user.id = :userId AND q.statusApproval = :isApproved AND q.department.id = :departmentId")
    Page<QuestionEntity> findByUserIdAndStatusApprovalAndDepartment(@Param("userId") Integer userId, @Param("isApproved") boolean isApproved, @Param("departmentId") Integer departmentId, Pageable pageable);

    // Lọc câu hỏi đã trả lời
    @Query("SELECT q FROM QuestionEntity q WHERE q.user.id = :userId AND EXISTS (SELECT a FROM AnswerEntity a WHERE a.question.id = q.id)")
    Page<QuestionEntity> findAnsweredQuestions(@Param("userId") Integer userId, Pageable pageable);

    // Lọc câu hỏi chưa trả lời
    @Query("SELECT q FROM QuestionEntity q WHERE q.user.id = :userId AND NOT EXISTS (SELECT a FROM AnswerEntity a WHERE a.question.id = q.id)")
    Page<QuestionEntity> findNotAnsweredQuestions(@Param("userId") Integer userId, Pageable pageable);

    // Lọc câu hỏi công khai
    @Query("SELECT q FROM QuestionEntity q WHERE q.user.id = :userId AND q.statusPublic = :isPublic")
    Page<QuestionEntity> findByUserIdAndStatusPublic(@Param("userId") Integer userId, @Param("isPublic") boolean isPublic, Pageable pageable);

    // Lọc câu hỏi bị xóa
    @Query("SELECT q FROM QuestionEntity q WHERE q.user.id = :userId AND q.statusDelete = :isDeleted")
    Page<QuestionEntity> findByUserIdAndStatusDelete(@Param("userId") Integer userId, @Param("isDeleted") boolean isDeleted, Pageable pageable);

    // Lọc câu hỏi đã duyệt
    @Query("SELECT q FROM QuestionEntity q WHERE q.user.id = :userId AND q.statusApproval = :isApproved")
    Page<QuestionEntity> findByUserIdAndStatusApproval(@Param("userId") Integer userId, @Param("isApproved") boolean isApproved, Pageable pageable);

    // Tìm tất cả câu hỏi theo userId
    @Query("SELECT q FROM QuestionEntity q WHERE q.user.id = :userId")
    Page<QuestionEntity> findByUserId(@Param("userId") Integer userId, Pageable pageable);

    // Tìm câu hỏi theo tiêu đề và phòng ban
    @Query("SELECT q FROM QuestionEntity q WHERE q.user.id = :userId AND q.title LIKE %:title% AND q.department.id = :departmentId")
    Page<QuestionEntity> searchQuestionsByTitleAndDepartment(@Param("userId") Integer userId, @Param("title") String title, @Param("departmentId") Integer departmentId, Pageable pageable);

    // Lọc câu hỏi theo phòng ban
    @Query("SELECT q FROM QuestionEntity q WHERE q.user.id = :userId AND q.department.id = :departmentId")
    Page<QuestionEntity> filterMyQuestionsByDepartment(@Param("userId") Integer userId, @Param("departmentId") Integer departmentId, Pageable pageable);
}

