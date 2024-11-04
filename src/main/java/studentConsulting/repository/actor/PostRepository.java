package studentConsulting.repository.actor;

import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.JpaSpecificationExecutor;
import org.springframework.data.repository.PagingAndSortingRepository;
import org.springframework.stereotype.Repository;
import studentConsulting.model.entity.PostEntity;

@Repository
public interface PostRepository extends PagingAndSortingRepository<PostEntity, Integer>, JpaSpecificationExecutor<PostEntity>, JpaRepository<PostEntity, Integer> {
}


