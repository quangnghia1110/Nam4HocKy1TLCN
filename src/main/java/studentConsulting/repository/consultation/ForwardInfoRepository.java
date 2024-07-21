package studentConsulting.repository.consultation;

import org.springframework.data.jpa.repository.JpaRepository;

import studentConsulting.model.entity.consultation.ForwardedInfoEntity;

public interface ForwardInfoRepository extends  JpaRepository<ForwardedInfoEntity, Integer>{

}
