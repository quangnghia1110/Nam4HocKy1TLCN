package studentConsulting.repository;

import java.util.Optional;

import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;
import org.springframework.stereotype.Repository;

import studentConsulting.model.entity.questionAnswer.AnswerEntity;

@Repository
public interface AnswerRepository extends JpaRepository<AnswerEntity, Integer> {

	@Query("SELECT a FROM AnswerEntity a WHERE a.question.id = :questionId ORDER BY a.createdAt ASC")
	Optional<AnswerEntity> findFirstAnswerByQuestionId(@Param("questionId") Integer questionId);

    boolean existsByQuestionId(Integer questionId);
    
    @Query("SELECT a FROM AnswerEntity a WHERE a.statusApproval = true AND a.statusAnswer = false")
    Page<AnswerEntity> findAnswersForReview(@Param("title") String title, Pageable pageable);

}
