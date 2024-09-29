package studentConsulting.repository.question_answer;

import org.springframework.data.jpa.repository.JpaSpecificationExecutor;
import org.springframework.data.repository.PagingAndSortingRepository;
import org.springframework.stereotype.Repository;
import studentConsulting.model.entity.question_answer.ForwardQuestionEntity;

@Repository
public interface ForwardQuestionRepository extends PagingAndSortingRepository<ForwardQuestionEntity, Integer>, JpaSpecificationExecutor<ForwardQuestionEntity> {

}
