package studentConsulting.repository.content;

import org.springframework.data.jpa.repository.JpaSpecificationExecutor;
import org.springframework.data.repository.PagingAndSortingRepository;
import org.springframework.stereotype.Repository;
import studentConsulting.model.entity.content.PostEntity;

import java.util.List;

@Repository
public interface PostRepository extends PagingAndSortingRepository<PostEntity, Integer>, JpaSpecificationExecutor<PostEntity> {
    List<PostEntity> findByIsApprovedFalseAndUser_Id(Integer userId);

    List<PostEntity> findByIsApprovedFalse();

    List<PostEntity> findByIsApprovedTrue();

}


