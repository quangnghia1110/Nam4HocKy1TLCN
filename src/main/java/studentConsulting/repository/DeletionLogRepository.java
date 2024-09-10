package studentConsulting.repository;

import java.util.Optional;

import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;

import studentConsulting.model.entity.questionAnswer.DeletionLogEntity;

@Repository
public interface DeletionLogRepository extends JpaRepository<DeletionLogEntity, Integer> {
	Optional<DeletionLogEntity> findByQuestionId(Integer questionId);

}

