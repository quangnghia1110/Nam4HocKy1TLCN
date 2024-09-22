package studentConsulting.repository;

import java.util.List;

import org.springframework.data.jpa.repository.JpaSpecificationExecutor;
import org.springframework.data.repository.PagingAndSortingRepository;
import org.springframework.stereotype.Repository;

import studentConsulting.model.entity.news.PostEntity;

@Repository
public interface PostRepository extends  PagingAndSortingRepository<PostEntity, Integer>, JpaSpecificationExecutor<PostEntity> {
    List<PostEntity> findByIsApprovedFalseAndUser_Id(Integer userId);

}


