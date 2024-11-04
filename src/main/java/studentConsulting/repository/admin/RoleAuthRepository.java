package studentConsulting.repository.admin;

import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Modifying;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;
import org.springframework.stereotype.Repository;
import studentConsulting.model.entity.RoleAuthEntity;

@Repository
public interface RoleAuthRepository extends JpaRepository<RoleAuthEntity, Long> {

    @Query("SELECT t FROM RoleAuthEntity t WHERE t.user.id = :userId")
    RoleAuthEntity findByUserId(@Param("userId") Long userId);

    @Query("SELECT t FROM RoleAuthEntity  t WHERE t.tokenId = :tokenId")
    RoleAuthEntity findByTokenId(@Param("tokenId") String tokenId);

    @Modifying
    @Query("DELETE FROM RoleAuthEntity  t WHERE t.tokenId = :tokenId")
    int deleteTokenByTokenId(@Param("tokenId") String tokenId);
}
