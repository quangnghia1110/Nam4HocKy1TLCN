package studentConsulting.repository.questionAnswer;

import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;
import org.springframework.stereotype.Repository;

import studentConsulting.model.entity.questionAnswer.CommonQuestionEntity;

@Repository
public interface CommonQuestionRepository extends  JpaRepository<CommonQuestionEntity, Integer>{
	 
}
