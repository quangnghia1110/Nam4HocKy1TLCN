package studentConsulting.repository;

import java.util.List;

import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;

import studentConsulting.model.entity.questionAnswer.QuestionEntity;

public interface QuestionRepository extends JpaRepository<QuestionEntity, Integer> {
	Page<QuestionEntity> findByUserId(Integer userId, Pageable pageable);

	@Query("SELECT q FROM QuestionEntity q WHERE q.user.id = :userId AND LOWER(q.title) LIKE LOWER(CONCAT('%', :title, '%'))")
	Page<QuestionEntity> findByUserIdAndTitle(@Param("userId") Integer userId, @Param("title") String title, Pageable pageable);

	Page<QuestionEntity> findByUserIdAndDepartmentId(Integer userId, Integer departmentId, Pageable pageable);

	Page<QuestionEntity> findByDepartmentId(Integer departmentId, Pageable pageable);

	@Query("SELECT q FROM QuestionEntity q " + "WHERE q.user.id = :userId "
			+ "AND EXISTS (SELECT a FROM AnswerEntity a WHERE a.question.id = q.id)")
	Page<QuestionEntity> findAnsweredQuestions(@Param("userId") Integer userId, Pageable pageable);

	@Query("SELECT q FROM QuestionEntity q " + "WHERE q.user.id = :userId "
			+ "AND NOT EXISTS (SELECT a FROM AnswerEntity a WHERE a.question.id = q.id)")
	Page<QuestionEntity> findNotAnsweredQuestions(@Param("userId") Integer userId, Pageable pageable);

	@Query("SELECT q FROM QuestionEntity q " + "WHERE q.user.id = :userId " + "AND q.statusPublic = :isPublic")
	Page<QuestionEntity> findByUserIdAndStatusPublic(@Param("userId") Integer userId,
			@Param("isPublic") Boolean isPublic, Pageable pageable);

	@Query("SELECT q FROM QuestionEntity q " + "WHERE q.user.id = :userId " + "AND q.statusApproval = :isApproved")
	Page<QuestionEntity> findByUserIdAndStatusApproval(@Param("userId") Integer userId,
			@Param("isApproved") Boolean isApproved, Pageable pageable);

	@Query("SELECT q FROM QuestionEntity q " + "WHERE q.user.id = :userId " + "AND q.statusDelete = :isDeleted")
	Page<QuestionEntity> findByUserIdAndStatusDelete(@Param("userId") Integer userId,
			@Param("isDeleted") Boolean isDeleted, Pageable pageable);

}
