package studentConsulting.repository;

import java.util.Optional;

import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;
import org.springframework.stereotype.Repository;

import studentConsulting.model.entity.authentication.RoleEntity;

@Repository
public interface RoleRepository extends JpaRepository<RoleEntity, Integer> {

    RoleEntity findByName(String name);
    
    @Query("SELECT r FROM RoleEntity r WHERE r.name = :name")
    Optional<RoleEntity> findByRoleName(@Param("name") String name);

}
